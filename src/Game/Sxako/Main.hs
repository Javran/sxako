module Game.Sxako.Main where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Board as Board
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Types
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako

pieceToFontChar :: Piece -> Char
pieceToFontChar (c, pt) = cs !! pInd
  where
    pInd = fromEnum pt
    cs = case c of
      White -> "pnbrqk"
      Black -> "omvtwl"

{-
  TODO: to fix the background, one possible way is to hard-code trail indices.
  See details in: https://groups.google.com/g/diagrams-discuss/c/r8ePb2ZhPq8
 -}
renderPiece :: PreparedFont Double -> Piece -> Diagram B
renderPiece font p = d # fc black # lw 0
  where
    d = strokeP $ textSVG' opts [ch]
    opts = TextOpts font INSIDE_H KERN False 1 70
    ch = pieceToFontChar p

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
          Just p -> square 70 <> center (renderPiece font p)

mainRender :: IO ()
mainRender = do
  let bd = placement dragonRecord
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (renderBoard font bd)
  pure ()

mainCmd :: IO ()
mainCmd = pprBoard (placement initRecord)

main :: IO ()
main = mainRender
