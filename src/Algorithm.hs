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
type ObservedState = Array U DIM3 Float

{-|
  Holds disparity value for each target pixel
        Z :. T_x :. T_y 
-}
type DisparityMeans a = Array a DIM2 Int

type Offsets a = Array a DIM1 Int

data DynamicNetwork a b = DynamicNetwork {
        network :: MarkovNet a,
        dispMap :: DisparityMeans b,
        offsets :: Offsets b
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
initDisparityMap width height = computeP $ R.fromFunction (ix2 width  height) (\_ -> 0)

initDynamicNetwork :: Int -> Int -> Int -> IO(DynamicNetwork U U)
initDynamicNetwork width height nDisparities = do
    net <- initMarkovNetwork width height nDisparities
    disp <- initDisparityMap width height
    let offs = disparityOffsets disp 4 nDisparities
    return (DynamicNetwork net disp offs)


disparityCompatibility :: DisparityCompatibility
disparityCompatibility ds dt = if dx < t then (1-e_p)*exp(-(dx/sigma_p))+e_p else e_p
        where
        e_p = 0.05
        sigma_p = 0.65
        t = 50
        dx = abs(fromIntegral(ds - dt))

initObservedStates :: Array U DIM2 Float -> Array U DIM2 Float -> IO(ObservedState)
initObservedStates imgLeft imgRight = computeP $ traverse
        imgLeft
        (\(Z:.h:.w) -> (Z:.w:.h:.w))
        (\_ (Z:.x:.y:.d) -> (1-e_d)*exp(-(abs(f x y d)/sigma_d))+e_d)
        where
        e_d = 0.01
        sigma_d = 0.3125
        f x y d = if x >= d then imgLeft ! (Z :. y :. x) - imgRight ! (Z :. y :. x - d) else 0.5

updateMessages :: Array U DIM2 Float -> DynamicNetwork U U -> DisparityCompatibility -> ObservedState -> IO(DynamicNetwork U U)
updateMessages lastDiff net dispCompat observedState = do
        let dispMap' = dispMap net
        let offsets' = offsets net
        network' <- computeP $ traverse (network net) id (lazyMessage lastDiff net dispCompat observedState (width, height, nDisparities))
        return (DynamicNetwork network' dispMap' offsets')
        where
        (Z :. width :. height :. _ :. nDisparities) = extent (network net)

disparities :: Source a Float => Source b Int => DynamicNetwork a b -> ObservedState -> IO(DisparityMeans U)
disparities net observedState = computeP $ traverse
                                           (dispMap net)
                                           id
                                           (\_ (Z:.x:.y) -> (disparityValue net x y (maxDisp x y)))
                                           where
                                           (Z:._:._:._:.nDisparities) = extent (network net)
                                           maxDisp x y = argmax (belief net observedState nDisparities x y) [0..nDisparities-1]

normaliseNet :: DynamicNetwork U U -> ObservedState -> IO(DynamicNetwork U U)
normaliseNet net state = do
        dispMap' <- shiftDisparities net
--        dispMap' <- disparities net state
        let (Z:._:._:._:.ndisp) = extent (network net)
        let offsets' = offsets net
--            dispMap' = dispMap net
--            shiftedNet = network net
        shiftedNet <- adjustShiftedDisparityLikelihoods net dispMap'
        ds <- sumP shiftedNet
        net' <- computeP $ traverse 
            shiftedNet 
            id 
            (\f (Z:.x:.y:.sd:.d) -> f (Z:.x:.y:.sd:.d) / ds!(Z:.x:.y:.sd))
        return (DynamicNetwork net' dispMap' offsets')

networkDiff :: DynamicNetwork U U -> DynamicNetwork U U -> IO(Array U DIM2 Float)
networkDiff a b = do
    diff :: MarkovNet U <- computeP $ (network a) -^ (network b)
    sqDiff :: MarkovNet U <- computeP $ diff *^ diff
    r1 <- sumP $ sqDiff
    sumP r1


-- Private Loopy Belief Propagation functions:


shiftDisparities :: Source a Float => Source b Int => DynamicNetwork a b -> IO(DisparityMeans U)
shiftDisparities net = do
    let (Z:._:._:._:.ndisp) = extent (network net)
        middle = floor(fromIntegral(ndisp)/(2::Float))
    let probForPoint x y d = product [(network net)!(Z:.x:.y:.k:.d) | k <- [0..3]]
        maxD x y = argmax (probForPoint x y) [0..ndisp-1]
    computeP $ traverse 
        (dispMap net)
        id 
        (\f (Z:.x:.y) ->
            let newD = maxD x y
                newDProb = probForPoint x y newD
                oldProb = probForPoint x y middle
            in if abs(newD-middle)<=1 || (newDProb*(0.9^((newD-middle)^2)) < oldProb) || (newDProb < 0.00001) then f(Z:.x:.y)
            else traceShow (x, y, newD, disparityValue net x y newD, newDProb, oldProb) disparityValue net x y newD
        )


adjustShiftedDisparityLikelihoods :: Source a Float => Source b Int => DynamicNetwork a b -> DisparityMeans b -> IO(MarkovNet U)
adjustShiftedDisparityLikelihoods net newDisparities = do
    let (Z:._:._:._:.ndisp) = extent (network net)
        newDisp = disparityValueForMeans net newDisparities
        oldDisp = disparityValue net
        dispDiff x y d = (newDisp x y d) - (oldDisp x y d)
        newValueLevelInOldDispMap x y d = disparityLevelFromValue net (dispMap net) x y (newDisp x y d)
    computeP $ traverse 
        (network net)
        id
        (\f (Z:.x:.y:.sd:.d) -> 
            if ((dispMap net)!(Z:.x:.y) == newDisparities!(Z:.x:.y)) then f(Z:.x:.y:.sd:.d)
            else f(Z:.x:.y:.sd:.(newValueLevelInOldDispMap x y d))
        )


belief :: Source a Float => Source b Int => DynamicNetwork a b -> ObservedState -> Int -> Int -> Int -> Int -> Float
belief net observedState nDisparities x y d = stateValue * probForPoint 
    where
        probForPoint = product [(network net)!(Z :. x :. y :. k :. d) | k <- [0..3]]
        disp = disparityValue net x y d
        (Z:.width:._) = extent (dispMap net)
        stateValue = if (disp<0) || (disp >= width) then 0 else observedState!(Z:.x:.y:.disp)


lazyMessage ::  Source a Float => Source b Int => Array U DIM2 Float -> DynamicNetwork a b -> DisparityCompatibility -> ObservedState -> (Int, Int, Int) -> (DIM4 -> Float) -> DIM4 -> Float
lazyMessage lastDiff disparitiesMap dynnet observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T) =
        case sourceCoordinates (width, height) tx ty sd of
                Just (sx, sy) ->
                    let alpha = 0.0001
                    in if lastDiff!(Z:.sx:.sy) < alpha then
                        net(Z:.tx:.ty:.sd:.d_T)
                    else
                        newMessage disparitiesMap dynnet observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T)
                Nothing -> 1

newMessage :: Source a Float => Source b Int => DynamicNetwork a b -> DisparityCompatibility -> ObservedState -> (Int, Int, Int) -> (DIM4 -> Float) -> DIM4 -> Float
newMessage dynnet disparityCompat observedState (width, height, nDisparities) net (Z :. tx :. ty :. sd :. d_T) =
        case sourceCoordinates (width, height) tx ty sd of
                Just (sx, sy) ->
                        let observed d = if (d < 0) || (d >= maxDisp) then 0 else observedState!(Z:.sx:.sy:.d)
                            currStepValue d_S = disparityValue dynnet sx sy d_S
                            fadingFactor d = 1 -- 0.999^(d*d) -- TODO: gaussian * image width
                            maxObserved d_S = let current = currStepValue d_S
                                                  lRange = levelRange dynnet d_S
                                in maximum (
                                    [(fadingFactor d) * observed (current+d) | d <- [-lRange..lRange]]
                                )
                            energy d_S = disparityCompat (disparityValue dynnet sx sy d_S) (disparityValue dynnet tx ty d_T)
                                * maxObserved d_S
                                * product [net (Z :. sx :. sy :. k :. d_S) | k <- [0..3], k /= inverseRelation sd]
                        in maximum [energy _d_shift | _d_shift<-[0..nDisparities-1]]
                Nothing -> 1
        where
            disparitiesMap = dispMap dynnet
            maxDisp = floor(fromIntegral width / (4::Float))

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


disparityOffsets :: Source a Int => DisparityMeans a -> Float -> Int -> Offsets U
disparityOffsets disparityMap coverage nLevels = R.fromListUnboxed (Z:.nLevels) (Prelude.map offset [0 .. nLevels-1])
    where 
        (Z:.width:._) = extent disparityMap
        middle = floor(fromIntegral nLevels / 2::Float)
        offset i = if (i == middle) then 0 
                    else if (i == middle-1) then -1
                    else if (i == middle+1) then  1
                    else round(fromIntegral width / coverage * currentLevelRatio / maxLevelRatio) * sign
                where
                    currentLevelRatio = (0.5 + fromIntegral i - (fromIntegral nLevels)/2::Float) ^ (2::Integer)
                    maxLevelRatio = (fromIntegral(nLevels)/2::Float) ^ (2::Integer)
                    sign =  if i*2 > nLevels then 1 else -1

disparityValue :: Source a Float => Source b Int => DynamicNetwork a b -> Int -> Int -> Int -> Int
disparityValue net x y level = disparityValueForMeans net (dispMap net) x y level

disparityValueForMeans :: Source a Float => Source b Int => DynamicNetwork a b -> DisparityMeans b -> Int -> Int -> Int -> Int
disparityValueForMeans net disparityMap x y level = disp
    where 
        (Z:.width:._) = extent disparityMap
        offs = offsets net
        disp = disparityMap!(Z:.x:.y) + offs!(Z:.level)

levelRange :: Source a Float => Source b Int => DynamicNetwork a b -> Int -> Int
levelRange net level = if minLevelDiff > 0 then minLevelDiff - 1 else 0
    where
        offs = offsets net
        (Z:.len) = extent offs
        prev = level - 1
        next = level + 1
        minLevelDiff = if prev<0 then abs(offs!(Z:.level) - offs!(Z:.next))
                       else if next>=len then abs(offs!(Z:.level) - offs!(Z:.prev))
                       else min (abs(offs!(Z:.level) - offs!(Z:.prev))) (abs(offs!(Z:.level) - offs!(Z:.next)))

disparityLevelFromValue :: Source a Float => Source b Int => DynamicNetwork a b -> DisparityMeans b -> Int -> Int -> Int -> Int
disparityLevelFromValue net disparityMap x y disparity = dispLevel 
    where
    offs = offsets net
    (Z:.len) = extent offs
    dispShift = disparity - disparityMap!(Z:.x:.y)
    middle = floor(fromIntegral(len)/(2::Float))
    dispLevelSuitable l = ((disparityValueForMeans net disparityMap x y l)+(levelRange net l) >= disparity) && ((disparityValueForMeans net disparityMap x y l)-(levelRange net l) <= disparity)
    findSuitableLevel [] = error "empty list passed where the last element should be the default value"
    findSuitableLevel [l] = l
    findSuitableLevel (l:rest) = if dispLevelSuitable l then l else findSuitableLevel rest
    dispLevel = if disparity < 0 then 0
                else if dispShift < 0 then (findSuitableLevel ([middle-k | k<-[1..middle]] ++ [0]))
                else if dispShift > 0 then (findSuitableLevel ([middle+k | k<-[1..middle]] ++ [len-1]))
                else 0

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





