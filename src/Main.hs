{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad
import           CV.ColourUtils
import           CV.DrawableInstances()
import           CV.Features
import           CV.Filters
import           CV.HighGUI
import           CV.Image
import           CV.ImageMathOp
import           CV.ImageOp
import           CV.Transforms
import           System.Environment
import           Utils.DrawingClass

delit :: Image GrayScale D32 -> Image GrayScale D32
delit i = i #- gaussian (41,41) i

main :: IO ()
main = do
   Just x <- liftM
        (fmap (stretchHistogram . gaussian (13,13) . delit))
        (getArgs >>= loadImage . head)
   let y = rotate (pi/2) x
       lst  = getSURF defaultSURFParams (unsafeImageTo8Bit x) Nothing
       lsty = getSURF defaultSURFParams (unsafeImageTo8Bit y) Nothing
   let result _lst _x = (_x <## map (draw.fst) _lst) :: Image GrayScale D32
   mapM_ print (take 5 lst)
   display $ scaleToSize Area True (1024, 750) $ montage (2,2) 3 [x, result lst x, result lsty y]
   return ()


