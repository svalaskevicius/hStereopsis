{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Data.Array.Repa.IO.Matrix
import           GlossRepa
import           Graphics.Gloss
import           System.Environment

main :: IO()
main = do
        args    <- getArgs
        case args of
                [fileName] -> run fileName
                _ -> putStr $ unlines [ "usage: ... " ]

run :: FilePath -> IO ()
run fileName = do
        img <- readRepaImage fileName
        let greyImg = toGrayscale img
        let (sbMag, thetas) = sobel greyImg
            (width, height, picture) = repaToPicture True (floatToGrayscale sbMag)
        writeMatrixToTextFile "test.dat" thetas
        putStrLn ("x: " Prelude.++ show width Prelude.++ ", Y: " ++ show height)
        display (InWindow fileName (width, height) (10,  10)) white picture

