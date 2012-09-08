{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module GlossRepa
  ( loadDevILPicture
  , repaToPicture
  , readRepaImage
  , toGrayscale
  , floatToGrayscale
  , sobel
  ) where

import           Control.Monad
import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa                 as R
import           Data.Array.Repa.Eval            as RE
import           Data.Array.Repa.IO.DevIL        as RD
import           Data.Array.Repa.Repr.ForeignPtr as R
import           Data.Word
import qualified Graphics.Gloss                  as G
import           Data.Array.Repa.Stencil         as R
import           Data.Array.Repa.Stencil.Dim2    as R

force
  :: ( RE.Load r1 sh e, RE.Target r2 e, Source r2 e)
  => Array r1 sh e -> Array r2 sh e
force = runIdentity . computeP


-- |Load  picture using 'Codec-Image-DevIL' and convert it a bitmap for display by 'Gloss'
loadDevILPicture :: Bool -> FilePath -> IO (Int, Int, G.Picture)
loadDevILPicture cache = liftM (repaToPicture cache) . readRepaImage

-- the number of columns, rows and a bitmap for use with 'Gloss'.
repaToPicture :: Bool -> RD.Image -> (Int, Int, G.Picture)
repaToPicture cache = multiChanToPicture cache . toRgba

toGrayscale :: RD.Image -> RD.Image 
toGrayscale (RD.Grey arr) = RD.Grey arr
toGrayscale (RD.RGB arr) = RD.Grey
        (force $ R.traverse
                arr
                (\ (Z :. i :. j :. _) -> (Z :. i :. j))
                (\f (Z :. i :. j) -> rgbToGrayScale (f (Z :. i :. j :. 2)) (f (Z :. i :. j :. 1)) (f (Z :. i :. j :. 0)))
        )
toGrayscale (RD.RGBA arr) = RD.Grey
        (force $ R.traverse
                arr
                (\ (Z :. i :. j :. _) -> (Z :. i :. j))
                (\f (Z :. i :. j) -> rgbToGrayScale (f (Z :. i :. j :. 3)) (f (Z :. i :. j :. 2)) (f (Z :. i :. j :. 1)))
        )

floatToGrayscale :: Array U DIM2 Float -> RD.Image 
floatToGrayscale arr = RD.Grey (force $ R.map (truncate.(*256)) arr)
        
toRgba :: RD.Image -> RD.Image
toRgba (RD.Grey arr) = RD.RGBA (grayToRgba arr)
toRgba (RD.RGB arr) = RD.RGBA (rgbToRgba arr)
toRgba (RD.RGBA arr) = RD.RGBA arr


-- |Read in a file into a repa array (using the 'repa-devil' package)
readRepaImage :: FilePath -> IO RD.Image
readRepaImage = RD.runIL . RD.readImage






gradientX :: (Source a Word8) => Array a DIM2 Word8 -> Array PC5 DIM2 Float
gradientX img = mapStencil2 (BoundConst 0) stencil $ R.map ((/ (3*256)).fromIntegral) img
        where stencil = [stencil2| -1  0  1
                                   -2  0  2
                                   -1  0  1 |]

gradientY :: (Source a Word8) => Array a DIM2 Word8 -> Array PC5 DIM2 Float
gradientY img = mapStencil2 (BoundConst 0) stencil $ R.map ((/ (3*256)).fromIntegral) img
        where stencil = [stencil2| 1  2  1
                                   0  0  0
                                  -1 -2 -1 |] 


sobel :: RD.Image -> (Array U DIM2 Float, Array U DIM2 Float)
sobel (RD.Grey img) = (force magnitudes, force thetas)
        where
        gx = gradientX img
        gy = gradientY img
        thetaFromDiff x y = if x == 0 then if y < 0 then -pi else if y > 0 then pi else 0 else atan (y / x)
        magnitudes = R.zipWith (\x y -> sqrt (x * x + y * y)) gx gy
        thetas = R.zipWith thetaFromDiff gx gy
sobel _ = error "sobel operator only works on grayscale images"



rgbToGrayScale :: Word8 -> Word8 -> Word8 -> Word8
rgbToGrayScale r g b = truncate (((0.2989 :: Float) * fromIntegral r) + (0.5870 * fromIntegral g) + (0.1140 * fromIntegral b))


        
        
rgbToRgba :: Array F DIM3 Word8 -> Array F DIM3 Word8
rgbToRgba arr =
        force
        $ R.traverse 
                arr 
                (\ (Z :. i :. j :. 3) -> (Z :. i :. j :. 4))
                (\f (Z :. i :. j :. c) -> case c of
                        0 -> 255
                        _ -> f (Z :. i :. j :. c-1)
                )


grayToRgba :: Array F DIM2 Word8 -> Array F DIM3 Word8
grayToRgba arr =
        force
        $ R.traverse 
                arr 
                (\ (Z :. i :. j) -> (Z :. i :. j :. 4))
                (\f (Z :. i :. j :. c) -> case c of
                        0 -> 255
                        _ -> f (Z :. i :. j)
                )

multiChanToPicture :: Bool -> RD.Image -> (Int, Int, G.Picture)
multiChanToPicture cache (RD.RGBA arr) = (col, row, glossPic)
        where
                Z :. row :. col :. _ = extent arr
                glossPic = G.bitmapOfForeignPtr col row
                        (R.toForeignPtr arr)
                        cache
multiChanToPicture _ _ = error "unsopported format passed to multiChanToPicture"
