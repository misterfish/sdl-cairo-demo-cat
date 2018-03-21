module Graphics.DemoCat.CairoSDL ( createSurfacePair ) where

import           Text.Printf ( printf )

import           Data.Vector.Storable.Mutable as VSM ( IOVector )
import           Data.Word ( Word8 )

import           GHC.Ptr ( castPtr )
import           Linear ( V2 (..)
                        , _x
                        , _y )

import           Foreign.C.String ( peekCString )
import           Foreign.C ( CInt (CInt) )

import qualified Graphics.Rendering.Cairo
    as C ( Format (FormatRGB24, FormatARGB32)
         , Surface
         , createImageSurfaceForData )

import qualified SDL
    as S ( Surface
         , PixelFormat (ARGB4444, RGB888, RGBA8888, ARGB8888, BGR888)
         , createRGBSurface
         , createRGBSurfaceFrom
         , loadBMP
         , lockSurface
         , unlockSurface
         , surfacePixels
         , surfaceDimensions )

import           Graphics.DemoCat.Types ( Log (Log, info, warn, err) )
import           Graphics.DemoCat.Util ( checkError )

import           Prelude hiding ( log )

createCSFromSS :: Log -> S.Surface -> IO C.Surface
createCSFromSS log surf = do
    let checkError' = checkError log
    checkError' "dimensions"
    dimensions <- S.surfaceDimensions surf
    let (width', height') = wh' dimensions
        stride' = 4 * width'
        wh' (V2 x y) = (fromIntegral x, fromIntegral y)
    putStrLn . printf "width: %d height: %d stride: %d" width' height' $ stride'
    checkError' "lockSurface"
    S.lockSurface surf
    checkError' "getPixels"
    pixelData <- castPtr <$> S.surfacePixels surf
    checkError' "createCairo"
    C.createImageSurfaceForData
        pixelData C.FormatARGB32 width' height' stride'

-- ignore `pixels`
createSurfacePair :: Log -> Bool -> IOVector Word8 -> IO (S.Surface, C.Surface)
createSurfacePair log doBg pixels = do
    let checkError' = checkError log
    -- | sdl surface: no transparency -- simpler to paste cairo surface,
    -- whose alpha pixels are pre-multiplied, straight in.
    sdlSurf <- makeSdlSurf' doBg
    cairoSurf <- createCSFromSS log sdlSurf
    checkError' "create cairo surf"
    pure (sdlSurf, cairoSurf) where
        makeSdlSurf' False = S.createRGBSurface (V2 800 600) S.ARGB8888
        makeSdlSurf' True = S.loadBMP "bandits.bmp"
        -- makeSdlSurf' True = S.createRGBSurfaceFrom pixels (V2 1280 696) (CInt $ 1280 * 3) S.RGB888
        -- makeSdlSurf' True = S.createRGBSurfaceFrom pixels (V2 1280 100) (CInt $ 1280 * 4) S.ARGB8888
        -- makeSdlSurf' True = S.createRGBSurfaceFrom pixels (V2 1280 696) (CInt $ 1280 * 3) S.BGR888
