{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Main where

import qualified Data.Map.Strict as M
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Board as Board
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Types
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako

{-
  TODO: to fix the background, one possible way is to hard-code trail indices.
  See details in: https://groups.google.com/g/diagrams-discuss/c/r8ePb2ZhPq8
 -}
renderPiece :: PreparedFont Double -> Piece -> Diagram B
renderPiece font p =
  strokeP chessPath # fc black # lw 0 <> strokeLocTrail outline # fc white # lw none
  where
    chessPath :: Path V2 Double
    chessPath = textSVG' opts [ch]
    opts = TextOpts font INSIDE_H KERN False 1 70
    (ch, bgInd) = meridaMeta M.! p
    outline = pathTrails chessPath !! bgInd

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

{-
  Metadata accompanying with ChessMerida font.
 -}
meridaMeta
  :: M.Map
       Piece
       ( {- char corresponding to the Chess piece -}
         Char
       , {- Trail index of the outline -}
         Int
       )
meridaMeta =
  M.fromList
    [ ((Black, Pawn), ('p', 3))
    , ((Black, Knight), ('n', 5))
    , ((Black, Bishop), ('b', 13))
    , ((Black, Rook), ('r', 1))
    , ((Black, Queen), ('q', 21))
    , ((Black, King), ('k', 3))
    , ((White, Pawn), ('o', 1))
    , ((White, Knight), ('m', 9))
    , ((White, Bishop), ('v', 9))
    , ((White, Rook), ('t', 1))
    , ((White, Queen), ('w', 11))
    , ((White, King), ('l', 9))
    ]

mainFindTrailIndices :: IO ()
mainFindTrailIndices = do
  let pieceChars = "pnbrqkomvtwl"
  fp <- getDataFileName "data/ChessMerida.svg"
  lFont <- lin @Double
  font <- loadFont @Double fp
  let opts = TextOpts font INSIDE_H KERN False 1 70
      paths :: [Path V2 Double]
      paths = fmap (\ch -> textSVG' opts [ch]) pieceChars
      pathComponents :: Path V2 Double -> Diagram B
      pathComponents p =
        hcat $
          (\(i, t) ->
             (strokeP
                (let opts' = TextOpts lFont INSIDE_H KERN False 1 20
                  in textSVG' opts' (show i))
                # lw 1
                # alignBL)
               <> strokeLocTrail t # fc red # lw none
               <> square 70 # lw 1 # bg white)
            <$> zip [0 :: Int ..] (pathTrails p)
  mainWith (vcat $ fmap pathComponents paths)

main :: IO ()
main = mainRender
