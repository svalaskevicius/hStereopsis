{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Algorithm
import           Data.Array.Repa.IO.Matrix
import           Graphics.Gloss
import           ImageFormat
import           System.Environment
import Control.Monad
import           Data.Array.Repa      as R            hiding ((++))

main :: IO()
main = do
        args    <- getArgs
        case args of
                [fileName1, fileName2] -> run fileName1 fileName2
                _ -> putStr $ unlines [ "usage: left right" ]

run :: FilePath -> FilePath -> IO ()
run fileNameLeft fileNameRight = do
        greyImgLeft <- liftM (grayscaleToFloat . toGrayscale) $ readRepaImage fileNameLeft
        greyImgRight <- liftM (grayscaleToFloat . toGrayscale) $ readRepaImage fileNameRight
        let (Z:.width:.height) = extent greyImgLeft

        putStrLn ("x: "++ show width ++ ", Y: " ++ show height)

        net <- initMarkovNetwork width height 1
        stateData <- initObservedStates 1 greyImgLeft greyImgRight
        --putStrLn (show stateData)
        net <- updateMessages net disparityCompatibility (retrieveObservedState stateData)
        -- let    (width, height, picture) = repaToPicture True (floatToGrayscale greyImgLeft)
       --     (width, height, picture) = (((repaToPicture True).floatToGrayscale.(gaussian 7 7).grayscaleToFloat) greyImg)
  --      writeMatrixToTextFile "test.dat" thetas
        -- display (InWindow fileNameLeft (width, height) (10,  10)) white picture
        return()
