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
        (width, height, picture) <- loadDevILPicture True fileName
        putStrLn ("x: " ++ show width ++ ", Y: " ++ show height)

        animate (InWindow fileName (width, height) (10,  10))
                black (frame width height picture)

frame :: Int -> Int -> Picture -> Float -> Picture
frame width height picture t
        = Color (greyN (abs $ sin (t * 2) / 4))
        $ Pictures
                [rectangleSolid (fromIntegral width) (fromIntegral height)
                , picture]
