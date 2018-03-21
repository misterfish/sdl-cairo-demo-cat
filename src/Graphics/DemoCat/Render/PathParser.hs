module Graphics.DemoCat.Render.PathParser ( parseAndRender
                                          , parseAndShow
                                          ) where

import           Data.Text ( Text )
import           Graphics.Rendering.Cairo ( Render )

import           Data.Monoid ( (<>) )

import           Text.ParserCombinators.Parsec
                 ( char
                 , digit
                 , satisfy
                 , try
                 , string
                 , optional
                 , option
                 , many
                 , many1
                 , oneOf
                 , sepBy
                 , sepBy1
                   -- can sometimes help resolve the trailing space problem.
                 , sepEndBy1
                 , GenParser
                 , ParseError
                 , eof
                 , (<|>)
                 , noneOf
                 , parse
                 )

import           Graphics.DemoCat.Render.Types
                 ( S (S)
                 , Operation (RelMoveTo, MoveTo, RelCurveTo, CurveTo, ClosePath)
                 , render )

import           Graphics.DemoCat.Render.Util ( trim, sp )

parseAndRender :: String -> Render ()
parseAndRender = render . either' . parseInput where
    either' = either (error "bad parse") id

-- | for dev
parseAndShow :: String -> String
parseAndShow = show . either' . parseInput where
    either' = either (error "bad parse") id

parseInput :: String -> Either ParseError S
parseInput = parse start "(unknown)" . trim

start :: GenParser Char st S
start = S <$> operations where
    operations = concat <$> sepEndBy1 operation sp'
    sp' = many1 sp

closePath :: GenParser Char st [Operation]
closePath = do
    char'
    pure [ClosePath] where
        char' = try (char 'z') <|> try (char 'Z')

-- xxx an m at the beginning means absolute.

operation :: GenParser Char st [Operation]
operation =  try moveTo
         <|> try relMoveTo
         <|> try relCurveTo
         <|> try curveTo
         <|> try closePath

moveTo :: GenParser Char st [Operation]
moveTo = do
    char 'M'
    many1 sp
    (x, y) <- pair
    pure [MoveTo x y]

relMoveTo :: GenParser Char st [Operation]
relMoveTo = do
    char 'm'
    many1 sp
    (x, y) <- pair
    pure [RelMoveTo x y]

triples :: GenParser Char st [(Double, Double, Double, Double, Double, Double)]
triples = do
    p1 <- triplePair
    ps <- many . try $ sp' *> triplePair
    pure $ p1 : ps where
        sp' = many1 sp

relCurveTo :: GenParser Char st [Operation]
relCurveTo = do
    char 'c'
    many1 sp
    -- sepBy doesn't work because it eats the following space as well
    -- sepEndBy1 also doesn't seem to work.
    -- pairs <- sepEndBy1 triplePair (try $ many1 sp)
    pairs <- triples
    pure . map relCurve' $ pairs where
        relCurve' (p1x, p1y, p2x, p2y, p3x, p3y) = RelCurveTo p1x p1y p2x p2y p3x p3y

curveTo :: GenParser Char st [Operation]
curveTo = do
    char 'C'
    many1 sp
    pairs <- triples
    pure . map curve' $ pairs where
        curve' (p1x, p1y, p2x, p2y, p3x, p3y) = CurveTo p1x p1y p2x p2y p3x p3y

triplePair :: GenParser Char st (Double, Double, Double, Double, Double, Double)
triplePair = do
    (p1x, p1y) <- pair
    many1 sp
    (p2x, p2y) <- pair
    many1 sp
    (p3x, p3y) <- pair
    pure (p1x, p1y, p2x, p2y, p3x, p3y)

pair :: GenParser Char st (Double, Double)
pair = do
    d1 <- double
    char ','
    d2 <- double
    pure (d1, d2)

double :: GenParser Char st Double
double = read' <$> (try pos' <|> try neg' <|> try plain') where
    read' :: String -> Double
    read' = read
    pos' = char '+' *> plain'
    neg' = (:) <$> char '-' <*> plain'
    plain' = (++) <$> many1 digit <*> option "" dec'
    dec' = (:) <$> char '.' <*> many1 digit
