{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}

module Graphics.DemoCat.Launch where

import           Data.ByteString as BS ( ByteString )

import           Control.Monad.IO.Class ( liftIO )
import           Foreign.C.String ( peekCString )
import           Text.Printf ( printf )
import           Data.Foldable ( find )
import           Control.Monad ( (<=<)
                               , unless
                               , when )
import           Control.Concurrent ( threadDelay )

import           SDL as S ( ($=)
                , Renderer
                , Surface
                , RendererType (SoftwareRenderer, AcceleratedRenderer)
                , PixelFormat (ARGB4444, RGB888)
                , EventPayload (KeyboardEvent)
                , InputMotion (Pressed)
                , KeyboardEventData
                , Texture
                , BlendMode (BlendNone, BlendAlphaBlend, BlendAdditive, BlendMod)
                , createRGBSurface
                , destroyTexture
                , eventPayload
                , keyboardEventKeyMotion
                , keysymKeycode
                , keyboardEventKeysym
                , copy
                , present
                , unlockSurface
                , initializeAll
                , createWindow
                , defaultWindow
                , windowInitialSize
                , defaultRenderer
                , rendererType
                , rendererTargetTexture
                , get
                , textureBlendMode
                , rendererDrawColor
                , pollEvents
                , createTextureFromSurface
                , createRenderer )

import qualified SDL.Raw.Error as SRE ( getError
                                      , clearError )

import           Linear ( V2 (..)
                        , V4 (..) )

import           SDL.Input.Keyboard.Codes ( pattern KeycodeQ )

import           Graphics.Rendering.Cairo.Matrix as CM ( Matrix (Matrix) )

import qualified Graphics.Rendering.Cairo as C ( Surface
                                               , Render
                                               , renderWith
                                               , setSourceRGBA
                                               , setSourceRGB
                                               , moveTo
                                               , lineTo
                                               , surfaceFlush
                                               , fill
                                               , transform
                                               , rectangle
                                               , stroke )

import           Data.Vector.Storable.Mutable as VSM ( IOVector )
import           Data.Word ( Word8 )

import qualified Graphics.DemoCat.CairoSDL as HC ( createSurfacePair )

import qualified Graphics.DemoCat.Render.Render as HRR ( renderFrames )
import           Graphics.DemoCat.Types ( Log (Log, info, warn, err)
                                        , Logger )
import           Graphics.DemoCat.Util ( checkError )

import           Prelude hiding ( log )

numRepeats = 5
takeFrames = 50

-- ignore `pixels`
launch :: (Logger, Logger, Logger) -> (Bool, IOVector Word8) -> IO ()
launch (androidLog, androidWarn, androidError) (bgOnly, pixels) = do

    let log = Log androidLog androidWarn androidError
        checkError' = checkError log
        info' = info log

    checkError' "begin"
    initializeAll
    checkError' "initializeAll"
    window <- createWindow "wuddup" $
        defaultWindow { windowInitialSize = V2 480 800 }
    checkError' "createWindow"
    renderer <- createRenderer window (-1) $
        defaultRenderer { rendererTargetTexture = False
                        , rendererType = AcceleratedRenderer }
    checkError' "createRenderer"
    (sdlSurf, cairoSurf) <- HC.createSurfacePair log bgOnly pixels
    framesRight <- HRR.renderFrames False
    framesLeft <- HRR.renderFrames True
    let frames' = concat . repeat $ frames''
        frames'' = take takeFrames framesRight ++ take takeFrames framesLeft
    appLoop renderer (sdlSurf, cairoSurf) frames' bgOnly 0
    return ()

getTexture renderer (sdlSurf, cairoSurf) renderFrame bgOnly t = do
    unless bgOnly $ C.renderWith cairoSurf cairoRender

    -- unlockSurface sdlSurf
    -- checkError' "unlock surface"

    tex <- createTextureFromSurface renderer sdlSurf
    -- checkError' "createTextureFromSurface"

    -- xxx destroy surf

    pure tex where
        cairoRender = do
            clear
            translateY . fromIntegral $ t
            translateX . fromIntegral $ t + 2
            renderFrame
        clear = do
            C.setSourceRGBA 0 0 0 1
            C.rectangle 0 0 1000 1000
            C.fill

appLoop :: Renderer -> (S.Surface, C.Surface) -> [C.Render ()] -> Bool -> Int -> IO ()
appLoop renderer (sdlSurf, cairoSurf) renderFrames bgOnly t = do
    tex <- getTexture renderer (sdlSurf, cairoSurf) (head renderFrames) bgOnly t
    events <- pollEvents
    -- clear renderer
    let source' = Nothing
        target' = Nothing -- | Nothing, i.e. NULL, means whole rectangle
    copy renderer tex source' target'
    destroyTexture tex
    present renderer
    let qPressed = any eventIsQPress events
        reloop = threadDelayMs 50 >> appLoop renderer (sdlSurf, cairoSurf) (tail renderFrames) bgOnly t'
        t' = (t + 1) `mod` (numRepeats * takeFrames)
    unless qPressed reloop

    return ()

eventIsQPress = event' . eventPayload where
    event' (KeyboardEvent kev) = allPass [pressed', q'] kev
    event' _ = False
    pressed' = (== Pressed) . keyboardEventKeyMotion
    q' = (== KeycodeQ) . keysymKeycode . keyboardEventKeysym

allPass :: [a -> Bool] -> a -> Bool
allPass fs x = all' fs where
    all' = check' . find not'
    not' f = not . f $ x
    check' (Just _) = False
    check' _ = True

threadDelayMs = threadDelay . (* 1000)

translateX :: Double -> C.Render ()
translateX = C.transform . translateXMatrix

translateY :: Double -> C.Render ()
translateY = C.transform . translateYMatrix

translateYMatrix = CM.Matrix 1 0 0 1 0
translateXMatrix x = CM.Matrix 1 0 0 1 x 0
