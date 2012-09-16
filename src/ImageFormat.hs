{-# LANGUAGE FlexibleContexts #-}

module ImageFormat
  ( loadDevILPicture
  , repaToPicture
  , readRepaImage
  , toGrayscale
  , floatToGrayscale, grayscaleToFloat
  ) where

import           Control.Monad
import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa                 as R
import           Data.Array.Repa.Eval            as RE
import           Data.Array.Repa.IO.DevIL        as RD
import           Data.Array.Repa.Repr.ForeignPtr as R
import           Data.Word
import qualified Graphics.Gloss                  as G


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

floatToGrayscale :: (Source a Float) => Array a DIM2 Float -> RD.Image 
floatToGrayscale arr = RD.Grey (force $ R.map (truncate.(*255)) arr)

grayscaleToFloat :: RD.Image -> Array D DIM2 Float
grayscaleToFloat (RD.Grey img) = R.map ((/ 255) . fromIntegral) img
grayscaleToFloat _ = error "non grayscale format provided to grayscaleToFloat"

        
toRgba :: RD.Image -> RD.Image
toRgba (RD.Grey arr) = RD.RGBA (grayToRgba arr)
toRgba (RD.RGB arr) = RD.RGBA (rgbToRgba arr)
toRgba (RD.RGBA arr) = RD.RGBA arr


-- |Read in a file into a repa array (using the 'repa-devil' package)
readRepaImage :: FilePath -> IO RD.Image
readRepaImage = RD.runIL . RD.readImage






rgbToGrayScale :: Word8 -> Word8 -> Word8 -> Word8
rgbToGrayScale r g b = round (((0.2989 :: Float) * fromIntegral r) + (0.5870 * fromIntegral g) + (0.1140 * fromIntegral b))


        
        
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
