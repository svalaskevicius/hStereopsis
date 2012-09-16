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
        putStrLn "reading images"
        imgLeft <- readRepaImage fileNameLeft
        imgRight <- readRepaImage fileNameRight
        putStrLn "transforming images"
        (floatImgLeft::Array U DIM2 Float) <- computeP $ (grayscaleToFloat . toGrayscale) imgLeft
        (floatImgRight::Array U DIM2 Float) <- computeP $ (grayscaleToFloat . toGrayscale) imgRight
        (smallFloatImgLeft::Array U DIM2 Float) <- computeP $ downSample 80 floatImgLeft
        (smallFloatImgRight::Array U DIM2 Float) <- computeP $ downSample 80 floatImgRight
        let transform = (sobel . gaussian 3 0.5)
            (d_greyImgLeft,_) = transform smallFloatImgLeft
            (d_greyImgRight,_) = transform smallFloatImgRight 
        greyImgLeft <- computeP d_greyImgLeft
        greyImgRight <- computeP d_greyImgRight

        runIL $ do
                writeImage "left.png" (floatToGrayscale greyImgLeft)
                writeImage "right.png" (floatToGrayscale greyImgRight)
        let (Z:.height:.width) = extent greyImgLeft
            nDisparities = 8

        putStrLn ("W: "++ show width ++ ", H: " ++ show height)

        putStrLn "init network"
        net <- initMarkovNetwork width height nDisparities
        putStrLn "init state data"
        stateData <- initObservedStates nDisparities greyImgLeft greyImgRight

        net' <- runNet 100 net stateData

        disp <- disparities net' (retrieveObservedState stateData)
        max_ <- foldAllP max 0 disp        
        let (dispMap::Array U DIM2 Float) = computeS $ R.map (\x-> fromIntegral x / fromIntegral max_) disp
        putStrLn ("D: "++ show disp ++ " m: "++show max_++" " ++ show dispMap)
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

