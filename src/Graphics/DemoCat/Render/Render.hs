{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
module Graphics.DemoCat.Render.Render ( renderFrames ) where

import           Data.Monoid ( (<>) )
import           Data.Maybe ( fromMaybe )
import           Data.Text ( Text
                           , unpack
                           , split
                           , index
                           , pack )
import           Data.List ( unfoldr
                           , intercalate )
import           Control.Monad ( when )
import           Control.Monad.Loops ( unfoldrM )
import           System.Random        ( getStdGen
                                      , randoms
                                      )

import           Graphics.Rendering.Cairo ( Render
                                          , fill
                                          , scale
                                          , rectangle
                                          , setSourceRGBA
                                          , setLineWidth
                                          , arc
                                          , stroke
                                          , moveTo
                                          , lineTo
                                          , curveTo
                                          , relCurveTo
                                          , transform
                                          , status )

import           Graphics.Rendering.Cairo.Matrix as CM ( Matrix (Matrix)
                                                       , rotate
                                                       , invert
                                                       , adjoint
                                                       , identity
                                                       )

import           Graphics.DemoCat.Render.Paths ( paths )
import           Graphics.DemoCat.Render.PathParser ( parseAndRender
                                                    , parseAndShow )

numLoops = 10
frameDuration = 50
horizontalVelocity = -15
scaleVelocity = 1 / 30
scaleVelocityFlip = 1 / 60
opacityStart = 0.3
opacityVelocity = 0.01
forceFlip = False
translateXAmount = 10
translateXAmountFlip = -3

renderFrames :: Bool -> IO [Render ()]
renderFrames flip' = do
    let paths' = paths
        paths'' = concat . replicate numLoops $ paths'
    pure . frames flip' [0..] $ paths''

-- | return stream of random doubles from [-1, 1]
-- same stream returned per process, and not sure about edges or even
-- distribution.
randoms' :: IO [Double]
randoms' = do
    gen <- getStdGen
    return . map divider' . randoms $ gen where
        divider' :: Int -> Double
        divider' n = fromIntegral n / fromIntegral (maxBound :: Int)

clamp :: Ord a => a -> a -> a -> a
clamp x y = max x . min y

flipHorizontal :: Render ()
flipHorizontal = transform . flipHorizontalMatrix $ 2880

translateX :: Double -> Render ()
translateX = transform . translateXMatrix

translateY :: Double -> Render ()
translateY = transform . translateYMatrix

flipHorizontalMatrix width = CM.Matrix (-1) 0 0 1 1440 0
translateXMatrix x  = CM.Matrix 1 0 0 1 x 0
translateYMatrix    = CM.Matrix 1 0 0 1 0

frames :: Bool -> [Int] -> [String] -> [Render ()]
-- frames ns = map frame' . zip ns where
frames flip' = zipWith (curry frame') where
    frame' (t, path') = do
        rgb 113 83 9 opacity'
        -- moveTo 0 0; lineTo 2440 900; stroke
        scale scalex' scaley'
        translateX . (* translateX'') $ t'
        when flip' flipHorizontal
        parseAndRender path'
        fill
        stroke where
            scalex' | flip' = clamp 0.1 1 $ 1 - t' * scaleVelocityFlip
                    | otherwise = 1 + t' * scaleVelocity
            translateX'' | flip' = translateXAmountFlip
                         | otherwise = translateXAmount
            scaley' = scalex'
            opacity' = 1
            xxxopacity' | flip' = clamp opacityStart 1 $ 1 - opacityVelocity * t'
                     | otherwise = clamp opacityStart 1 $ opacityStart + opacityVelocity * t'
            t' = fromIntegral t
    rgb r g b = setSourceRGBA (r / 255) (g / 255) (b/ 255)

renderNoop :: Render ()
renderNoop = setSourceRGBA 0 0 0 0

minus b a = a - b

-- lazysig :: IO ()
-- lazysig = getLine >>= putStrLn . either'' . generate . either' . parseInput where
--     either' = either (error "bad parse") id
--     either'' = either (error . (++) "bad generate: ") id

-- | get substring, using inclusive indices.
-- ok if y is too big (caps automatically).
-- "" if x is too big.
substr :: (Int, Int) -> Text -> Text
substr (x, y) = pack . take (y - x + 1) . snd . splitAt x . unpack

pathsTest = take 2 [ "m 141.8,10.7"
            , "M 162.00,135.00 \n\
              \C 166.30,146.25 174.44,152.60 184.00,159.28\n\
              \  188.05,162.11 192.94,165.51 198.00,165.88\n\
              \  202.60,166.22 209.18,163.93 214.00,163.08\n\
              \  221.56,161.76 226.43,161.91 234.00,162.00\n\
              \  245.77,162.15 250.65,168.23 260.00,167.94\n\
              \  266.12,167.75 268.66,164.80 274.00,162.81\n\
              \  274.00,162.81 286.00,159.00 286.00,159.00\n\
              \  288.83,162.84 295.27,168.46 293.36,173.89\n\
              \  291.68,178.68 286.67,176.84 280.01,181.11\n\
              \  275.83,183.79 273.69,187.28 270.56,191.00\n\
              \  263.84,198.98 259.78,200.63 261.43,212.00\n\
              \  261.72,214.01 262.20,217.21 263.17,218.94\n\
              \  265.72,223.50 268.46,221.79 273.00,226.00\n\
              \  259.07,232.15 251.03,215.19 249.00,204.00\n\
              \  249.00,204.00 239.87,218.01 239.87,218.01\n\
              \  239.87,218.01 224.00,224.47 224.00,224.47\n\
              \  224.00,224.47 215.00,219.34 215.00,219.34\n\
              \  215.00,219.34 208.74,212.96 208.74,212.96\n\
              \  208.74,212.96 208.00,201.00 208.00,201.00\n\
              \  202.83,203.54 190.14,210.52 190.41,216.99\n\
              \  190.54,220.02 193.52,222.96 195.00,226.00\n\
              \  185.84,225.46 185.80,222.80 184.82,215.00\n\
              \  184.82,215.00 183.62,209.00 183.62,209.00\n\
              \  183.15,203.84 187.02,201.63 188.49,197.00\n\
              \  189.61,192.53 187.28,190.03 188.49,185.00\n\
              \  189.52,181.76 192.69,177.60 191.52,174.82\n\
              \  189.81,170.76 181.99,169.49 178.00,167.01\n\
              \  169.40,161.66 157.29,145.72 159.00,135.00\n\
              \  159.00,135.00 162.00,135.00 162.00,135.00 Z\n\
              \M 220.19,205.00\n\
              \C 218.54,206.90 215.93,209.93 217.15,212.63\n\
              \  217.68,213.78 219.10,214.80 220.06,215.57\n\
              \  224.65,219.26 226.39,220.53 231.98,217.80\n\
              \  233.54,217.04 235.03,216.25 235.98,214.73\n\
              \  237.18,212.79 237.97,200.83 238.00,198.00\n\
              \  229.10,198.12 226.51,197.75 220.19,205.00 Z\n\
              \"

            , "c 1,1 2,2 3,3"
            , "m 141.8,10.7 m 141.8,10.7"
            , "m 141.8,10.7 c 1,1 2,2 3,3"
            , "m 141.8,10.7 c 1,1 2,2 3,3 4,4 5,5 6,6"
            , "m 141.8,10.7 c 1,1 2,2 3,3 c 4,4 5,5 6,6"
            , "m 141.8,10.7 m 1,1"
            , "m 141.8,10.7 c 1,1 2,2 3,3 4,4 5,5 6,6"
            , "m 141.8,10.7 C 1,1 2,2 3,3 4,4 5,5 6,6"
            , "m 141.8,10.7 c -1.6,-0.7 -3.6,3.9 -5.4,5 0.1,-1.9 5.4,-6.7 3.8,-9.7 -0.5,2.2 -1.5,3.4 -2.3,5.2 -0.2,-1.9 1.4,-4.8 1.5,-6.9 -1.6,1.6 -2.1,5.6 -3.3,8.1 -1.2,-0.3 1.4,-2.5 -0.5,-2.5 -2.2,8.6 -6.2,13.5 -11.8,15.2 -7.6,5.7 -15.3,11.6 -23.2,17.6 c 1,1 2,2 3,3"
            , "m 141.8,10.7 c -1.6,-0.7 -3.6,3.9 -5.4,5 0.1,-1.9 5.4,-6.7 3.8,-9.7 -0.5,2.2 -1.5,3.4 -2.3,5.2 -0.2,-1.9 1.4,-4.8 1.5,-6.9 -1.6,1.6 -2.1,5.6 -3.3,8.1 -1.2,-0.3 1.4,-2.5 -0.5,-2.5 -2.2,8.6 -6.2,13.5 -11.8,15.2 -7.6,5.7 -15.3,11.6 -23.2,17.6 C 96.1,46.1 93,50.3 87.4,51.4 85.5,50.3 84.5,51 83,50.8 82.4,51.2 82.1,52.2 81.5,52.6 72.7,54.4 60.1,48.1 48.1,45.5 37.2,43.1 21.4,42.4 17.8,32.1 c -0.9,1.2 1.7,3.7 2.8,5.1 -3.9,-1.4 -6.2,-6.6 -9.1,-9.6 1.4,3.5 3.9,6.5 6.9,9.3 -3.1,-1 -6.5,-4.8 -9,-7.3 1.3,3.4 6.9,7.6 12.6,10 -2.7,0.4 -5.6,-2.1 -8.2,-2.9 2,1.9 5.2,3.2 8.6,4.4 -2.6,0.4 -6.7,-2.7 -9.3,-2.3 2.9,2.3 6.1,2.5 10.1,4 -2,1 -5.3,-0.6 -7.7,-0.4 0.9,1.3 3.7,0.7 5.7,2 -3.1,1 8.1,3.2 8.4,4.8 1.3,0.4 2.6,0.7 3.6,0.3 0.5,2.2 3.7,1.3 5.3,1.9 1.5,0.5 2.9,2.7 4.3,3.4 10.8,5 23.4,7.4 34.9,12.2 -3.5,4.5 -8.7,5.6 -12.8,8.9 2,0.9 5.2,2.6 7,-0.3 1.2,0.7 -0.2,2.6 0.1,3.7 1.4,0.8 2.5,0.9 4.4,2 0.8,-0.7 2.3,0 2.5,-1.9 0.7,0.4 -0.2,1.7 1.2,1.8 5.4,0.4 10.3,-0.4 13.3,-5 0.7,-0.9 2.1,-0.5 3,-0.8 1,-1.7 1,-3.7 3.5,-3.4 -1,-2.2 1,-2.8 2.3,-3.8 -1.9,-1.5 -7.1,-1.6 -9.3,-3.7 7.7,-11.5 18.4,-16.8 26.4,-27.7 5.4,-2.9 9.6,-8.4 14.2,-13 -0.4,0 -0.6,0.3 -1,0.1 0.2,-1 3.1,-2.5 4.5,-3.5 -0.9,-0.9 -3.1,1.5 -4.7,1.6 1.8,-1.6 3.4,-3.7 5.4,-5 -0.9,-0.8 -2.9,2.1 -4.2,2.7 1.4,-3.3 3.7,-3.5 8.3,-9"
            , "m 141.8,10.7 c -1.6,-0.7 -3.6,3.9 -5.4,5 0.1,-1.9 5.4,-6.7 3.8,-9.7 -0.5,2.2 -1.5,3.4 -2.3,5.2 -0.2,-1.9 1.4,-4.8 1.5,-6.9 -1.6,1.6 -2.1,5.6 -3.3,8.1 -1.2,-0.3 1.4,-2.5 -0.5,-2.5 -2.2,8.6 -6.2,13.5 -11.8,15.2 -7.6,5.7 -15.3,11.6 -23.2,17.6 C 96.1,46.1 93,50.3 87.4,51.4 85.5,50.3 84.5,51 83,50.8 82.4,51.2 82.1,52.2 81.5,52.6 72.7,54.4 60.1,48.1 48.1,45.5 37.2,43.1 21.4,42.4 17.8,32.1 c -0.9,1.2 1.7,3.7 2.8,5.1 -3.9,-1.4 -6.2,-6.6 -9.1,-9.6 1.4,3.5 3.9,6.5 6.9,9.3 -3.1,-1 -6.5,-4.8 -9,-7.3 1.3,3.4 6.9,7.6 12.6,10 -2.7,0.4 -5.6,-2.1 -8.2,-2.9 2,1.9 5.2,3.2 8.6,4.4 -2.6,0.4 -6.7,-2.7 -9.3,-2.3 2.9,2.3 6.1,2.5 10.1,4 -2,1 -5.3,-0.6 -7.7,-0.4 0.9,1.3 3.7,0.7 5.7,2 -3.1,1 8.1,3.2 8.4,4.8 1.3,0.4 2.6,0.7 3.6,0.3 0.5,2.2 3.7,1.3 5.3,1.9 1.5,0.5 2.9,2.7 4.3,3.4 10.8,5 23.4,7.4 34.9,12.2 -3.5,4.5 -8.7,5.6 -12.8,8.9 2,0.9 5.2,2.6 7,-0.3 1.2,0.7 -0.2,2.6 0.1,3.7 1.4,0.8 2.5,0.9 4.4,2 0.8,-0.7 2.3,0 2.5,-1.9 0.7,0.4 -0.2,1.7 1.2,1.8 5.4,0.4 10.3,-0.4 13.3,-5 0.7,-0.9 2.1,-0.5 3,-0.8 1,-1.7 1,-3.7 3.5,-3.4 -1,-2.2 1,-2.8 2.3,-3.8 -1.9,-1.5 -7.1,-1.6 -9.3,-3.7 7.7,-11.5 18.4,-16.8 26.4,-27.7 5.4,-2.9 9.6,-8.4 14.2,-13 -0.4,0 -0.6,0.3 -1,0.1 0.2,-1 3.1,-2.5 4.5,-3.5 -0.9,-0.9 -3.1,1.5 -4.7,1.6 1.8,-1.6 3.4,-3.7 5.4,-5 -0.9,-0.8 -2.9,2.1 -4.2,2.7 1.4,-3.3 3.7,-3.5 8.3,-9 z" ]
