{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Algorithm
--import           Graphics.Gloss
import           Data.Array.Repa          as R hiding ((++))
import           Data.Array.Repa.IO.DevIL
import           ImageFormat
import           System.Environment
import Data.Bits

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
        let downSampleFactor = minimumFactorForImageSize 32 floatImgLeft 

        (net, stateData) <- runNetHierarchy downSampleFactor Nothing floatImgLeft floatImgRight


--        let (_, _, picture) = repaToPicture True (floatToGrayscale greyImgLeft)
       --     (width, height, picture) = (((repaToPicture True).floatToGrayscale.(gaussian 7 7).grayscaleToFloat) greyImg)
  --      writeMatrixToTextFile "test.dat" thetas
--        display (InWindow fileNameLeft (width, height) (10,  10)) white picture
        return()

runNetHierarchy :: Int -> Maybe (MarkovNet U, Array U DIM3 Float) -> Array U DIM2 Float -> Array U DIM2 Float -> IO((MarkovNet U, Array U DIM3 Float))
runNetHierarchy 0 (Just (net, stateData)) _ _ = return (net, stateData)
runNetHierarchy factor maybeNetData imgLeft imgRight = do
        putStrLn ("Initialise hierarchy level "++show factor)
        
        (greyImgLeft, greyImgRight) <- initImages factor imgLeft imgRight
        runIL $ do
                writeImage ("left_"++show factor++".png") (floatToGrayscale greyImgLeft)
                writeImage ("right_"++show factor++".png") (floatToGrayscale greyImgRight)
        let (Z:.height:.width) = extent greyImgLeft
            nDisparities = 16

        putStrLn ("W: "++ show width ++ ", H: " ++ show height)

        putStrLn "init network data"
        net <- case maybeNetData of
            Just (sourceNet, _) -> scaleNet sourceNet width height
            Nothing -> initMarkovNetwork width height nDisparities
        stateData <- initObservedStates [i*4 | i<-[0..nDisparities-1]] greyImgLeft greyImgRight
        net' <- runNet 2500 Nothing net stateData

        writeDisparities net' stateData ("disparities_"++(show factor)++".png")

        return (net', stateData)
--        runNetHierarchy (factor `shiftR` 1) (Just (net', stateData)) imgLeft imgRight


runNet :: Int -> Maybe (Array U DIM2 Float) -> MarkovNet U -> Array U DIM3 Float -> IO(MarkovNet U)
runNet 0 _ net _ = return net
runNet times Nothing net state =
    runNet times (Just initialDiff) net state
    where
    (Z:.w:.h:.4:.d) = extent(net)
    initialDiff = fromListUnboxed (Z :. (w::Int) :. (h::Int)) [1 | i<-[1..w*h]]

runNet times (Just lastDiff) net state = do
        putStrLn ("running.. "++show times++" times left")
        net' <- updateMessages lastDiff net disparityCompatibility (retrieveObservedState state)
        net'' <- normaliseNet net'
        diff <- networkDiff net net''
        err' <- sumP diff
        err <- sumP err'
        putStrLn ("err: "++show err)
        runNet (if ((err!(Z)) > 0) then (times-1) else 0) (Just diff) net'' state 

initImages :: (Source a Float) => Int -> Array a DIM2 Float -> Array a DIM2 Float -> IO( (Array U DIM2 Float, Array U DIM2 Float) )
initImages factor left right = do
    leftRes <- prepareImage factor left
    rightRes <- prepareImage factor right
    return (leftRes, rightRes)

prepareImage :: (Source a Float) => Int -> Array a DIM2 Float -> IO(Array U DIM2 Float)
prepareImage factor img = do
    (smallImg::Array U DIM2 Float) <- computeP $ downSample factor img
    let transform = (gaussian 3 0.5)
        d_greyImg = transform smallImg
    computeP d_greyImg


writeDisparities :: MarkovNet U -> Array U DIM3 Float -> String -> IO()
writeDisparities net stateData fileName = do
    disp <- disparities net (retrieveObservedState stateData)
    max_ <- foldAllP max 0 disp
    let (dispMap::Array U DIM2 Float) = computeS $ R.map (\x-> fromIntegral x / fromIntegral max_) disp
    runIL $ writeImage fileName $ floatToGrayscale dispMap
