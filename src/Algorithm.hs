{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Algorithm
  (
        sobel
        , gaussian
        , initMarkovNetwork
        , updateMessages
  ) where

import           Data.Array.Repa                 as R
import           Data.Array.Repa.Stencil         as R
import           Data.Array.Repa.Stencil.Dim2    as R


{-|
  Define Markov Network for message passing of Loopy Belief Propagation
  The dimensions define values as:
        Z :. T_x :. T_y :. S_d :. Disparity
  Where S_d takes values according to the following schema
  depending on relative position from T:
    1
  2 T 3
    4
-}
type MarkovNet a = Array a DIM4 Float

{-|
  Calculate likelihood of disparity between source and target
  Parameters: T_x, T_y, S_d, D_s, D_t
  Where S_d takes values according to the following schema
  depending on relative position from T:
    1
  2 T 3
    4
-}
type DisparityCompatibility = (Int -> Int -> Int -> Int -> Int -> Float)

{-|
  Calculate data likelihood for Target position and given disparity
  Parameters: T_x, T_y, D
-}
type ObservedState = (Int -> Int -> Int -> Float)


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
        

initMarkovNetwork :: Int -> Int -> Int -> MarkovNet D
initMarkovNetwork width height nDisparities = R.fromFunction (ix4 width  height (4::Int) nDisparities) (\_ -> 1::Float)

updateMessages ::  Source a Float => MarkovNet a -> DisparityCompatibility -> ObservedState -> MarkovNet D
updateMessages net dispCompat observedState = traverse net id (newMessage dispCompat observedState nDisparities)
        where
        (Z :. _ :. _ :. _ :. nDisparities) = extent net

newMessage :: DisparityCompatibility -> ObservedState -> Int -> (DIM4 -> Float) -> DIM4 -> Float
newMessage disparityCompat observedState nDisparities network (Z :. tx :. ty :. sd :. d_T) = 
        maximum [energy d_S | d_S<-[0..nDisparities]]
        where
        (sx, sy) = sourceCoordinates tx ty sd
        energy d_S = disparityCompat tx ty sd d_S d_T 
                * observedState sx sy d_S 
                * product [network (Z :. sx :. sy :. k :. d_S) | k <- [1 .. 4], k /= inverseRelation sd]

inverseRelation :: Int -> Int
inverseRelation 1 = 4
inverseRelation 2 = 3
inverseRelation 3 = 2
inverseRelation 4 = 1
inverseRelation _ = error "inverseRelation:: unknown relation" 

sourceCoordinates :: Int -> Int -> Int -> (Int, Int)
sourceCoordinates tx ty 1 = (tx, ty-1) 
sourceCoordinates tx ty 2 = (tx-1, ty) 
sourceCoordinates tx ty 3 = (tx+1, ty) 
sourceCoordinates tx ty 4 = (tx, ty+1) 
sourceCoordinates _ _ _ = error "sourceCoordinates:: Cannot compute source position from the given sd" 


-- Private functions:
generateGaussKernel :: Int -> Float -> Stencil DIM2 Float
generateGaussKernel width sigma = makeStencil2 width width genStencil
        where
        center = (truncate.(/ (2::Float)).fromIntegral) width
        gaussianKernel = [exp(-( fromIntegral (x*x + y*y)/(2*sigma*sigma) )) | x <- [-center..center], y <- [-center..center]]
        normalisedGaussian = R.fromListUnboxed (Z :. width :. width) (Prelude.map (/ sum gaussianKernel) gaussianKernel)
        genStencil (Z:.x:.y) 
                | x >= -center && x <= center && y >= -center && y <= center = Just (normalisedGaussian!(Z:.x+center:.y+center))
                | otherwise = Nothing
         
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





