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

renderBoard :: PreparedFont Double -> Board -> Diagram B
renderBoard font bd = vcat (fmap renderRank fenCoords) # bg white
  where
    renderRank :: [Coord] -> Diagram B
    renderRank cs = hcat (fmap go cs)
      where
        go c = case Board.at bd c of
          Nothing -> square 70
          Just p -> square 70 <> center (renderPiece font p)

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
