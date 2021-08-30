module Game.Sxako.Main where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Types
import Game.Sxako.Board
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako
import Game.Sxako.Fen

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

chessPieces :: PreparedFont Double -> Diagram B
chessPieces font = vcat (fmap (\t -> stroke (textSVG' opts t) # fc black # lcA transparent) testTexts) # bg white
  where
    opts = TextOpts font INSIDE_H KERN False 1 70

mainRender :: IO ()
mainRender = do
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (chessPieces font)
  pure ()

main :: IO ()
main = pprBoard (fromPlacement (placement initRecord))
