module Main where

import Graphics.DemoCat.Launch ( launch )

bgOnly = False

main :: IO ()
main = do
    -- broken legacy interface
    let img = undefined
    launch (noop, noop, noop) (bgOnly, img) where
    noop = const . pure $ ()
