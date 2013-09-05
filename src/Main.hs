{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Prelude hiding (catch) 
import           Algorithm
--import           Graphics.Gloss
import           Data.Array.Repa          as R hiding ((++))
import           Data.Array.Repa.IO.DevIL
import           ImageFormat
import           System.Environment
import           Control.Exception

main :: IO()
main = do
        args    <- getArgs
        case args of
                [fileName1, fileName2] -> catch (run fileName1 fileName2) (\e -> do let err = show (e::SomeException)
                                                                                    putStr err
                                                                                    return ()
                                                                                    )
                _ -> putStr $ unlines [ "usage: left right" ]

run :: FilePath -> FilePath -> IO ()
run fileNameLeft fileNameRight = do
        putStrLn "reading images"
        imgLeft <- readRepaImage fileNameLeft
        imgRight <- readRepaImage fileNameRight
        putStrLn "transforming images"
        (floatImgLeft::Array U DIM2 Float) <- computeP $ (grayscaleToFloat . toGrayscale) imgLeft
        (floatImgRight::Array U DIM2 Float) <- computeP $ (grayscaleToFloat . toGrayscale) imgRight
        let downSampleFactor = minimumFactorForImageSize 212 floatImgLeft 

        _ <- calculateDisparities downSampleFactor floatImgLeft floatImgRight

        return()

calculateDisparities :: Int -> Array U DIM2 Float -> Array U DIM2 Float -> IO((DynamicNetwork U U, Array U DIM3 Float))
calculateDisparities factor imgLeft imgRight = do
        putStrLn ("Initialise hierarchy level "++show factor)
        
        (greyImgLeft, greyImgRight) <- initImages factor imgLeft imgRight
        runIL $ do
                writeImage ("left_"++show factor++".png") (floatToGrayscale greyImgLeft)
                writeImage ("right_"++show factor++".png") (floatToGrayscale greyImgRight)
        let (Z:.height:.width) = extent greyImgLeft
            nDisparities = 9

        putStrLn ("W: "++ show width ++ ", H: " ++ show height)

        putStrLn "init network data"
        net <- initDynamicNetwork width height nDisparities
        stateData <- initObservedStates greyImgLeft greyImgRight
        net' <- runNet 500 Nothing net stateData

        writeDisparities net' stateData ("disparities_"++(show factor)++".png")

        return (net', stateData)


runNet :: Int -> Maybe (Array U DIM2 Float) -> DynamicNetwork U U -> ObservedState -> IO(DynamicNetwork U U)
runNet 0 _ net _ = return net
runNet times Nothing net state =
    runNet times (Just initialDiff) net state
    where
    (Z:.w:.h:.4:._) = extent(network net)
    initialDiff = fromListUnboxed (Z :. (w::Int) :. (h::Int)) [1 | _<-[1..w*h]]

runNet times (Just lastDiff) net state = do
        putStrLn ("running.. "++show times++" times left")
        net' <- updateMessages lastDiff net disparityCompatibility state
        net'' <- normaliseNet net' state
        diff <- networkDiff net net''
        err' <- sumP diff
        err'' <- sumP err'
        let err = (err''!(Z)::Float)
        putStrLn ("diffErr: "++show err)
        runIL $ writeImage ("diff_"++(show times)++".png") $ floatToGrayscale diff
        runNet (if (err > 0.0001) then (times-1) else 0) (Just diff) net'' state 

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


writeDisparities :: DynamicNetwork U U -> Array U DIM3 Float -> String -> IO()
writeDisparities net stateData fileName = do
    disp <- disparities net stateData
    max_ <- foldAllP max 0 disp
    let (disparityMap::Array U DIM2 Float) = computeS $ R.map (\x-> fromIntegral x / fromIntegral max_) disp
    runIL $ writeImage fileName $ floatToGrayscale disparityMap

