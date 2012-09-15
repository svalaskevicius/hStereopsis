{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Algorithm
  (
        sobel
        , gaussian
  ) where

import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa                 as R
import           Data.Array.Repa.Eval            as R
import           Data.Array.Repa.Stencil         as R
import           Data.Array.Repa.Stencil.Dim2    as R


{-|
  Define Markov Network for message passing of Loopy Belief Propagation
  The dimensions define values as:
        Z :. S_x :. S_y :. T_d :. Disparity
  Where T_d takes values according to the following schema
  depending on relative position from S:
    1
  2 S 3
    4
-}
type MarkovNet a = Array a DIM4 Float

{-|
  Calculate likelihood of disparity between source and target
  Parameters: S_x, S_y, T_d, D
  Where T_d takes values according to the following schema
  depending on relative position from S:
    1
  2 S 3
    4
-}
type DisparityCompatibility = (Int -> Int -> Int -> Int -> Float)

{-|
  Calculate data likelihood for Source position and given disparity
  Parameters: S_x, S_y, D
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

updateMessages ::  Source a Float => MarkovNet a -> DisparityCompatibility -> ObservedState -> MarkovNet U
updateMessages net _ _ = force $ delay net


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



force
  :: ( R.Load r1 sh e, R.Target r2 e, Source r2 e)
  => Array r1 sh e -> Array r2 sh e
force = runIdentity . computeP



