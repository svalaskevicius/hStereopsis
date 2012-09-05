{-# LANGUAGE FlexibleContexts #-}
module Main where
--import           CV.Bindings.Types
import           CV.ColourUtils
import           CV.DrawableInstances
import           CV.Drawing
import           CV.Features
import           CV.Filters
import           CV.Image
import           CV.ImageMathOp
import           CV.ImageOp
import           CV.Transforms
import           System.Environment
import           System.IO.Unsafe
import           Utils.DrawingClass
--import           Utils.GeometryClass

delit :: Image GrayScale D32 -> Image GrayScale D32
delit i = i #- gaussian (41,41) i

main :: IO ()
main = do
   Just x <- getArgs >>= loadImage . head >>= return
    . fmap stretchHistogram
    . fmap (gaussian (13,13))
    . fmap delit
   let y = scale Area (2.2,2) . rotate (pi/2) $ x
       lst  = getSURF defaultSURFParams (unsafeImageTo8Bit x) Nothing
       lsty = getSURF defaultSURFParams (unsafeImageTo8Bit y) Nothing
   let result lst x = (x <## map (draw.fst) lst) :: Image GrayScale D32
  -- [ellipseBoxOp 1 (C'CvBox2D c size d) 1 0
  --                    | (C'CvSURFPoint c l s d h,_) <- lst
  --                    , let size = C'CvSize2D32f (fromIntegral s) (fromIntegral $ s`div`2)
  --                    ]
   saveImage "surf_result.png" $ montage (1,2) 2 [x, (result lst x)] -- ,result lsty y]
   mapM_ print (take 5 lst)

padToSize :: (CreateImage (Image GrayScale D32)) =>  (Int,Int)
                                                    -> Image GrayScale D32
                                                    -> Image GrayScale D32
padToSize size img = unsafePerformIO $ do
    let r = empty size
    blit r img (0,0)
    return r
