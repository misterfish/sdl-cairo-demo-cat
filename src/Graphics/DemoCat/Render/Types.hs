module Graphics.DemoCat.Render.Types ( S (S)
                                     , Operation ( RelMoveTo, MoveTo
                                                 , RelCurveTo, CurveTo
                                                 , ClosePath )
                                     , render
                                     ) where

import           Text.Printf ( printf )
import           Data.List ( intercalate )
import qualified Graphics.Rendering.Cairo as C ( Render
                                               , relMoveTo
                                               , moveTo
                                               , relCurveTo
                                               , closePath
                                               , curveTo )

data S = S [Operation]
data Operation = RelMoveTo Double Double
               | MoveTo Double Double
               | RelCurveTo Double Double Double Double Double Double
               | CurveTo Double Double Double Double Double Double
               | ClosePath

class Renderable a where
    -- render :: a -> Either String (C.Render ())
    render :: a -> C.Render ()

instance Renderable S where
    render (S operations) = mapM_ render operations

instance Renderable Operation where
    render (RelMoveTo x y) = C.relMoveTo x y
    render (MoveTo x y) = C.moveTo x y
    render (RelCurveTo x1 y1 x2 y2 x3 y3) = C.relCurveTo x1 y1 x2 y2 x3 y3
    render (CurveTo x1 y1 x2 y2 x3 y3) = C.curveTo x1 y1 x2 y2 x3 y3
    render ClosePath = C.closePath

instance Show S where
    show (S operations) = intercalate "\n" . map show $ operations

instance Show Operation where
    show (RelMoveTo x y) = printf "RelMoveTo (%f, %f)" x y
    show (MoveTo x y) = printf "MoveTo (%f, %f)" x y
    show (RelCurveTo x1 y1 x2 y2 x3 y3) = printf "RelCurveTo (%f, %f) (%f, %f) (%f, %f)" x1 y1 x2 y2 x3 y3
    show (CurveTo x1 y1 x2 y2 x3 y3) = printf "CurveTo (%f, %f) (%f, %f) (%f, %f)" x1 y1 x2 y2 x3 y3
    show ClosePath = printf "ClosePath"
