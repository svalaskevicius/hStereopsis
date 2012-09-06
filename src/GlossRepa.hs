-- from http://hackage.haskell.org/package/gloss-devil

module GlossRepa
  ( loadDevILPicture
  , repaToPicture
  , readRepaImage
  ) where

import           Control.Monad
import           Control.Monad.Identity (runIdentity)
import           Data.Array.Repa                 as R
import           Data.Array.Repa.Eval                 as RE
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
repaToPicture cache (RGBA arr) = multiChanToPicture cache arr
repaToPicture cache (RGB arr) =  multiChanToPicture cache (rgbToRgba arr)
repaToPicture _ (Grey _) = error "only rgb or rgba images are supported, got grey"

rgbToRgba :: Array F DIM3 Word8 -> Array F DIM3 Word8
rgbToRgba arr =
        force
        $ R.traverse 
                arr 
                (\ (Z :. i :. j :. c) -> (Z :. i :. j :. c+1))
                (\f (Z :. i :. j :. c) -> case c of
                        0 -> 255
                        _ -> f (Z :. i :. j :. c-1)
                )

multiChanToPicture :: Bool -> Array F DIM3 Word8 -> (Int, Int, G.Picture)
multiChanToPicture cache arr = (col, row, glossPic)
  where
  Z :. row :. col :. _ = extent arr
  glossPic = G.bitmapOfForeignPtr col row
                        (R.toForeignPtr arr)
                        cache

-- |Read in a file into a repa array (using the 'repa-devil' package)
readRepaImage :: FilePath -> IO RD.Image
readRepaImage = RD.runIL . RD.readImage

