module Game.Sxako.Main where

import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Types
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako
import Game.Sxako.Coord

testTexts =
  [ "tmvwlvmt"
  , "oooooooo"
  , "pppppppp"
  , "rnbqkbnr"
  ]

chessPieces :: PreparedFont Double -> Diagram B
chessPieces font = vcat (fmap (\t -> stroke (textSVG' opts t) # fc black # lcA transparent) testTexts) # bg white
  where
    opts = TextOpts font INSIDE_H KERN False 1 70

main :: IO ()
main = do
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (chessPieces font)
  pure ()

