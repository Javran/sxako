{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.Render
  ( subCmdMain
  )
where

import qualified Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude hiding (font)
import Game.Sxako.Board as Board
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.Fen
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont
import Paths_sxako
import System.Environment
import System.Exit

renderPiece :: PreparedFont Double -> Piece -> Diagram B
renderPiece font p =
  strokeP chessPath # fc black # lw 0 <> strokeP outline # fc white # lw none
  where
    chessPath :: Path V2 Double
    chessPath = [ch] # svgText opts # fit_height 70 # drop_rect
    opts = (def :: TextOpts Double) {textFont = font}
    (ch, bgInds) = meridaMeta M.! p
    outline = toPath (fmap (pathTrails chessPath !!) bgInds)

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

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    xs | ([fenRaw], _ : extraArgs) <- span (/= "--") xs -> do
      record <-
        if fenRaw == "startpos"
          then pure initRecord
          else case Parser.parseOnly fenP (BSC.pack fenRaw) of
            Left msg -> do
              putStrLn $ "Parse error: " <> msg
              exitFailure
            Right v -> pure v
      pprBoard (placement record)
      withArgs
        extraArgs
        (renderRecord record)
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<FEN> -- ..args to diagrams.."
      putStrLn "<FEN> could also be 'startpos'."
      exitFailure

renderRecord :: Record -> IO ()
renderRecord record = do
  let bd = placement record
  fp <- getDataFileName "data/ChessMerida.svg"
  font <- loadFont fp
  mainWith (renderBoard font bd)

{-
  Metadata accompanying with ChessMerida font.

  See details in: https://groups.google.com/g/diagrams-discuss/c/r8ePb2ZhPq8
 -}
meridaMeta
  :: M.Map
       Piece
       ( {- char corresponding to the Chess piece -}
         Char
       , {- Trail indices of the outline -}
         [Int]
       )
meridaMeta =
  M.fromList
    [ ((White, Pawn), ('p', [3]))
    , ((White, Knight), ('n', [5]))
    , ((White, Bishop), ('b', [13]))
    , ((White, Rook), ('r', [1]))
    , ((White, Queen), ('q', [1, 21, 25, 31, 33, 39]))
    , ((White, King), ('k', [3]))
    , ((Black, Pawn), ('o', [1]))
    , ((Black, Knight), ('m', [9]))
    , ((Black, Bishop), ('v', [9]))
    , ((Black, Rook), ('t', [1]))
    , ((Black, Queen), ('w', [11]))
    , ((Black, King), ('l', [9]))
    ]

_mainFindTrailIndices :: IO ()
_mainFindTrailIndices = do
  let pieceChars = "pnbrqkomvtwl"
  fp <- getDataFileName "data/ChessMerida.svg"
  lFont <- lin @Double
  font <- loadFont @Double fp
  let opts = (def :: TextOpts Double) {textFont = font}
      paths :: [Path V2 Double]
      paths = fmap (\ch -> [ch] # svgText opts # fit_height 70 # drop_rect) pieceChars
      pathComponents :: Path V2 Double -> Diagram B
      pathComponents p =
        hcat $
          (\(i, t) ->
             (strokeP
                (let opts' = (def :: TextOpts Double) {textFont = lFont}
                  in show i # svgText opts' # fit_height 20 # drop_rect)
                # lw 1
                # alignBL)
               <> strokeLocTrail t # fc red # lw none
               <> square 70 # lw 1 # bg white)
            <$> zip [0 :: Int ..] (pathTrails p)
  mainWith (vcat $ fmap pathComponents paths)
