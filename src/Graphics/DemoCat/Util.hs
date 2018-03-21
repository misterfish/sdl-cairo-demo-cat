module Graphics.DemoCat.Util ( checkError ) where

import           Text.Printf ( printf )
import           Foreign.C.String ( peekCString )
import qualified SDL.Raw.Error as SRE ( getError
                                      , clearError )

import           Graphics.DemoCat.Types ( Log (Log, info, warn, err) )

import           Prelude hiding ( log )

checkError :: Log -> String -> IO ()
checkError log tag = do
    check' =<< peekCString =<< SRE.getError
    SRE.clearError where
        check' "" = pure ()
        check' theError = err log theError


