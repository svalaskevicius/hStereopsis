{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Algorithm
  (
        minimumFactorForImageSize 
        , downSample
        , sobel
        , gaussian

        , initMarkovNetwork
        , initDisparityMap 
        , initDynamicNetwork 
        , updateMessages
        , disparityCompatibility
        , initObservedStates
        , retrieveObservedState
        , disparities
        , normaliseNet
        , networkDiff

        , DynamicNetwork , network, dispMap
        , DisparityCompatibility
        , ObservedState
  ) where

import           Data.Array.Repa              as R hiding ((++))
import           Data.Array.Repa.Stencil      as R
import           Data.Array.Repa.Stencil.Dim2 as R
import           Data.List.Extras.Argmax

{-|
  Define Markov Network for message passing of Loopy Belief Propagation
  The dimensions define values as:
        Z :. T_x :. T_y :. S_d :. Disparity shift (see disparityValue) from the mean
  Where S_d takes values according to the following schema
  depending on relative position from T:
    0
  3 T 1
    2
-}
type MarkovNet a = Array a DIM4 Float

{-|
  Calculate likelihood of disparity between source and target
  Parameters: D_s, D_t
-}
type DisparityCompatibility = (Int -> Int -> Float)

{-|
  Calculate data likelihood for Target position and given disparity
  Parameters: T_x, T_y, D
-}
type ObservedState = (Int -> Int -> Int -> Float)

{-|
  Holds disparity value for each target pixel
        Z :. T_x :. T_y 
-}
type DisparityMeans a = Array a DIM2 Int

data DynamicNetwork a b = DynamicNetwork {
        network :: MarkovNet a,
        dispMap :: DisparityMeans b
    }

-- | Find a downsampling factor of 2, so that both dimensions are smaller or equal than the given number in pixels
minimumFactorForImageSize :: (Source a Float) => Int -> Array a DIM2 Float -> Int
minimumFactorForImageSize maxDim img = 
                        round $ head [
                                (2::Float) ^ i
                                | i<-[(1::Integer)..],
                                (fromIntegral width / (2::Float) ^ i) < fromIntegral maxDim,
                                (fromIntegral height / (2::Float) ^ i) < fromIntegral maxDim
                                ]
                        where
                        (Z:.height:.width) = extent img


-- | Downsample the image by the given factor of 2
downSample :: (Source a Float) => Int -> Array a DIM2 Float -> Array D DIM2 Float
downSample factor img =
                        traverse img
                        (\(Z:.h:.w) -> (Z:.round((fromIntegral h :: Float) / fromIntegral factor):.round ((fromIntegral w :: Float)/fromIntegral factor)))
                        (\_ (Z:.y:.x) -> sumAllS (pixelSample y x) / fromIntegral (factor * factor))
                        where
                        pixelSample :: Int -> Int -> Array D DIM2 Float
                        pixelSample y x = fromFunction
                                        (Z:.factor:.factor)
                                        (\(Z:._y:._x) -> img!(Z:.y*factor+_y:.x*factor+_x))


sobel :: (Source a Float) => Array a DIM2 Float -> (Array D DIM2 Float, Array D DIM2 Float)
sobel img = (magnitudes, thetas)
        where
        gx = gradientX img
        gy = gradientY img
        thetaFromDiff x y = if x == 0 then if y < 0 then -pi else if y > 0 then pi else 0 else atan (y / x)
        magnitudes = R.zipWith (\x y -> sqrt ((x * x + y * y) / 9)) gx gy
        thetas = R.zipWith thetaFromDiff gx gy

gaussian :: (Source a Float) => Int -> Float -> Array a DIM2 Float -> Array D DIM2 Float
gaussian width sigma = delay . mapStencil2 BoundClamp (generateGaussKernel width sigma)



initMarkovNetwork :: Int -> Int -> Int -> IO(MarkovNet U)
initMarkovNetwork width height nDisparities = computeP $ R.fromFunction (ix4 width  height (4::Int) nDisparities) (\_ -> 1::Float)

initDisparityMap :: Int -> Int -> IO(DisparityMeans U)
initDisparityMap width height = computeP $ R.fromFunction (ix2 width  height) (\_ -> 0::Int)

initDynamicNetwork :: Int -> Int -> Int -> IO(DynamicNetwork U U)
initDynamicNetwork width height nDisparities = do
    net <- initMarkovNetwork width height nDisparities
    disp <- initDisparityMap width height
    return (DynamicNetwork net disp)


disparityCompatibility :: DisparityCompatibility
disparityCompatibility ds dt = if dx < t then (1-e_p)*exp(-(dx/sigma_p))+e_p else e_p
        where
        e_p = 0.05
        sigma_p = 0.6
        t = 4
        dx = abs(fromIntegral(ds - dt))

initObservedStates :: [Int] -> Array U DIM2 Float -> Array U DIM2 Float -> IO(Array U DIM3 Float)
initObservedStates dispList imgLeft imgRight = computeP $ traverse
        imgLeft
        (\(Z:.h:.w) -> (Z:.w:.h:.nDisparities))
        (\_ (Z:.x:.y:.d) -> (1-e_d)*exp(-(abs(f x y (dispList!!d))/sigma_d))+e_d)
        where
        nDisparities = length dispList
        e_d = 0.01
        sigma_d = 0.3125
        f x y d = if x >= d then imgLeft ! (Z :. y :. x) - imgRight ! (Z :. y :. x - d) else 1

retrieveObservedState :: Array U DIM3 Float -> ObservedState
retrieveObservedState stateMap tx ty d = stateMap!(Z:.tx:.ty:.d)

updateMessages :: Source a Float => Source b Int => Array U DIM2 Float -> DynamicNetwork a b -> DisparityCompatibility -> ObservedState -> IO(DynamicNetwork U b)
updateMessages lastDiff net dispCompat observedState = do
        let dispMap' = dispMap net
        network' <- computeP $ traverse (network net) id (lazyMessage lastDiff dispMap' dispCompat observedState (width, height, nDisparities))
        return (DynamicNetwork network' dispMap')
        where
        (Z :. width :. height :. _ :. nDisparities) = extent (network net)

disparities :: Source a Float => Source b Int => DynamicNetwork a b -> ObservedState -> IO(Array U DIM2 Int)
disparities net observedState = computeP $ traverse
                                           (network net)
                                           (\ (Z:.w:.h:.4:._) -> (Z:.h:.w))
                                           (\_ (Z:.y:.x) -> (argmax (belief (network net) observedState x y) [0..nDisparities-1]))
                                           where
                                           (Z:._:._:._:.nDisparities) = extent (network net)

normaliseNet :: DynamicNetwork U U -> IO(DynamicNetwork U U)
normaliseNet net = do
        ds <- sumP (network net)
        let (Z:._:._:._:.ndisp) = extent (network net)
            dispMap' = dispMap net
        net' <- computeP $ traverse (network net) id (\f (Z:.x:.y:.sd:.d) -> f (Z:.x:.y:.sd:.d) / ds!(Z:.x:.y:.sd) * fromIntegral ndisp)
        return (DynamicNetwork net' dispMap')

networkDiff :: DynamicNetwork U U -> DynamicNetwork U U -> IO(Array U DIM2 Float)
networkDiff a b = do
    diff :: MarkovNet U <- computeP $ (network a) -^ (network b)
    sqDiff :: MarkovNet U <- computeP $ diff *^ diff
    r1 <- sumP $ sqDiff
    sumP r1


-- Private Loopy Belief Propagation functions:

belief :: Source a Float => MarkovNet a -> ObservedState -> Int -> Int -> Int -> Float
belief net observedState x y d = observedState x y d * product [net!(Z :. x :. y :. k :. d) | k <- [0..3]]


lazyMessage :: Source a Int => Array U DIM2 Float -> DisparityMeans a -> DisparityCompatibility -> ObservedState -> (Int, Int, Int) -> (DIM4 -> Float) -> DIM4 -> Float
lazyMessage lastDiff disparitiesMap disparityCompat observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T) =
        case sourceCoordinates (width, height) tx ty sd of
                Just (sx, sy) ->
                    let alpha = 0.01
                    in if lastDiff!(Z:.sx:.sy) < alpha then
                        net(Z:.tx:.ty:.sd:.d_T)
                    else
                        newMessage disparitiesMap disparityCompat observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T)
                Nothing -> 1

newMessage :: Source a Int => DisparityMeans a -> DisparityCompatibility -> ObservedState -> (Int, Int, Int) -> (DIM4 -> Float) -> DIM4 -> Float
newMessage disparitiesMap disparityCompat observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T) =
        case sourceCoordinates (width, height) tx ty sd of
                Just (sx, sy) ->
                        let energy d_S = disparityCompat d_S d_T
                                * observedState sx sy d_S
                                * product [net (Z :. sx :. sy :. k :. d_S) | k <- [0..3], k /= inverseRelation sd]
                        in sum [energy d_S | d_S<-[0..nDisparities-1]]
                Nothing -> 1

inverseRelation :: Int -> Int
inverseRelation 0 = 2
inverseRelation 1 = 3
inverseRelation 2 = 0
inverseRelation 3 = 1
inverseRelation _ = error "inverseRelation:: unknown relation"

sourceCoordinates :: (Int, Int) -> Int -> Int -> Int -> Maybe (Int, Int)
sourceCoordinates (_, _) tx ty 0 = if ty > 0 then Just (tx, ty-1) else Nothing
sourceCoordinates (w, _) tx ty 1 = if tx < w-1 then Just (tx+1, ty) else Nothing
sourceCoordinates (_, h) tx ty 2 = if ty < h-1 then Just (tx, ty+1) else Nothing
sourceCoordinates (_, _) tx ty 3 = if tx > 0 then Just (tx-1, ty) else Nothing
sourceCoordinates _ _ _ _ = error "sourceCoordinates:: Cannot compute source position from the given sd"


disparityValue :: Source a Int => DisparityMeans a -> Int -> Int -> Int -> Int -> Int
disparityValue disparityMap nLevels x y level = disparityMap!(Z:.x:.y) + deltaDisp
    where
        (Z:.width:.height) = extent disparityMap
        maxSize = max width height
        coverage = 8 -- 1/8 coverage of max dimension
        currentLevelRatio = (fromIntegral level - ((fromIntegral nLevels)::Float) / 2::Float) ^ (3::Integer)
        maxLevelRatio = (((fromIntegral nLevels)::Float) / 2::Float) ^ (3::Integer)
        deltaDisp = floor(fromIntegral maxSize / coverage * currentLevelRatio / maxLevelRatio)


-- Private Gaussian Filter functions:

generateGaussKernel :: Int -> Float -> Stencil DIM2 Float
generateGaussKernel width sigma = makeStencil2 width width genStencil
        where
        center = (truncate.(/ (2::Float)).fromIntegral) width
        gaussianKernel = [exp(-( fromIntegral (x*x + y*y)/(2*sigma*sigma) )) | x <- [-center..center], y <- [-center..center]]
        normalisedGaussian = R.fromListUnboxed (Z :. width :. width) (Prelude.map (/ sum gaussianKernel) gaussianKernel)
        genStencil (Z:.x:.y)
                | x >= -center && x <= center && y >= -center && y <= center = Just (normalisedGaussian!(Z:.x+center:.y+center))
                | otherwise = Nothing


-- Private Sobel Filter functions:

gradientX :: (Source a Float) => Array a DIM2 Float -> Array PC5 DIM2 Float
gradientX = mapStencil2 BoundClamp stencil
        where stencil = [stencil2| -1  0  1
                                   -2  0  2
                                   -1  0  1 |]

gradientY :: (Source a Float) => Array a DIM2 Float -> Array PC5 DIM2 Float
gradientY = mapStencil2 BoundClamp stencil
        where stencil = [stencil2| 1  2  1
                                   0  0  0
                                  -1 -2 -1 |]





