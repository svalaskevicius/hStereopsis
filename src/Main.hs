{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Main where

import           Algorithm
import           Data.Array.Repa.IO.Matrix
import           Graphics.Gloss
import           ImageFormat
import           System.Environment
import Control.Monad
import           Data.Array.Repa      as R            hiding ((++))
import Data.Array.Repa.IO.DevIL

main :: IO()
main = do
        args    <- getArgs
        case args of
                [fileName1, fileName2] -> run fileName1 fileName2
                _ -> putStr $ unlines [ "usage: left right" ]

run :: FilePath -> FilePath -> IO ()
run fileNameLeft fileNameRight = do
        greyImgLeft <- liftM (gaussian 3 0.5 . downSample 80 . grayscaleToFloat . toGrayscale) $ readRepaImage fileNameLeft
        greyImgRight <- liftM (gaussian 3 0.5 . downSample 80 . grayscaleToFloat . toGrayscale) $ readRepaImage fileNameRight
        
        runIL $ do
                writeImage "left.png" (floatToGrayscale greyImgLeft)
                writeImage "right.png" (floatToGrayscale greyImgRight)
        let (Z:.width:.height) = extent greyImgLeft

        putStrLn ("x: "++ show width ++ ", Y: " ++ show height)

        putStrLn "init network"
        net <- initMarkovNetwork width height 16
        putStrLn "init state data"
        stateData <- initObservedStates 16 greyImgLeft greyImgRight

        net <- runNet 10 net stateData

        disp <- disparities net (retrieveObservedState stateData)
        putStrLn ("d: "++ show disp)
        
        max_ <- foldAllP max 0 disp
        (dispMap::Array U DIM2 Float) <- computeP $ traverse disp id (\f (Z:.x:.y)-> fromIntegral (f (Z:.x:.y)) / fromIntegral max_)
        runIL $ do
                writeImage "disp.png" $ dispMap `deepSeqArray` (floatToGrayscale dispMap)

--        let (_, _, picture) = repaToPicture True (floatToGrayscale greyImgLeft)
       --     (width, height, picture) = (((repaToPicture True).floatToGrayscale.(gaussian 7 7).grayscaleToFloat) greyImg)
  --      writeMatrixToTextFile "test.dat" thetas
--        display (InWindow fileNameLeft (width, height) (10,  10)) white picture
        return()
        

runNet :: Int -> Array U DIM4 Float -> Array U DIM3 Float -> IO(Array U DIM4 Float) 
runNet 0 net _ = return net 
runNet times net state = do
        putStrLn ("running "++show times++" times.")
        net1 <- updateMessages net disparityCompatibility (retrieveObservedState state)
        net2 <- normaliseNet net1
        runNet (times-1) net2 state

