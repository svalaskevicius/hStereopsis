{-# LANGUAGE FlexibleContexts #-}

module Main where

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
        let (width, height, picture) = repaToPicture True (toGrayscale img)

        putStrLn ("x: " Prelude.++ show width Prelude.++ ", Y: " ++ show height)
        display (InWindow fileName (width, height) (10,  10)) black picture

