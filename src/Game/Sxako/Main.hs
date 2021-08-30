module Game.Sxako.Main where

import Data.Colour.RGBSpace
import Data.Word
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Board as Board
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Types
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako

testTexts =
  [ "tmvwlvmt"
  , "oooooooo"
  , "pppppppp"
  , "rnbqkbnr"
  ]

-- TODO: not tested yet.
pieceToFontChar :: Piece -> Char
pieceToFontChar (c, pt) = cs !! pInd
  where
    pInd = fromEnum pt
    cs = case c of
      White -> "pnbrqk"
      Black -> "omvtwl"

renderPiece :: PreparedFont Double -> Piece -> Diagram B
renderPiece font p = stroke (textSVG' opts [ch]) # fc black # lcA transparent
  where
    opts = TextOpts font INSIDE_H KERN False 1 70
    ch = pieceToFontChar p

chessPieces :: PreparedFont Double -> Diagram B
chessPieces font = vcat (fmap (\t -> stroke (textSVG' opts t) # fc black # lcA transparent) testTexts) # bg white
  where
    opts = TextOpts font INSIDE_H KERN False 1 70

{-
  TODO: for now color bleeds to foreground, not sure how to fix yet.
 -}
renderBoard :: PreparedFont Double -> Board -> Diagram B
renderBoard font bd = vcat (fmap renderRank fenCoords) # bg white
  where
    darkC = sRGB24 0x88 0x77 0xb7
    lightC = sRGB24 0xef 0xef 0xef
    renderRank :: [Coord] -> Diagram B
    renderRank cs = hcat (fmap (\c -> go c # bg (if isDark c then darkC else lightC)) cs)
      where
        go c = case Board.at bd c of
          Nothing -> square 70
          Just p -> square 70 <> center (renderPiece font p # fc white)

mainRender :: IO ()
mainRender = do
  let bd = fromPlacement (placement dragonRecord)
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (renderBoard font bd)
  pure ()

mainCmd :: IO ()
mainCmd = pprBoard (fromPlacement (placement initRecord))

main :: IO ()
main = mainRender
