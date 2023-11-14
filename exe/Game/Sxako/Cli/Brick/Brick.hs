module Game.Sxako.Cli.Brick.Brick (
  main,
  mainWith,
  mainWithArgs,
) where

import Brick
import qualified Brick.BChan
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State
import Data.Coerce
import Data.Functor
import Data.List
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Time.LocalTime
import Dhall hiding (maybe, string, void)
import Game.Sxako.Board
import qualified Game.Sxako.Cli.Brick.ChessBomb as Cb
import qualified Game.Sxako.Cli.Brick.Config as Cfg
import Game.Sxako.Cli.Brick.Stockfish
import Game.Sxako.Cli.Brick.Stockfish.Types
import Game.Sxako.Common
import Game.Sxako.Coord (fenCoords)
import Game.Sxako.Fen
import qualified Graphics.Vty
import Graphics.Vty.Attributes (defAttr)
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit
import Text.Printf

pieceToCharUnicode :: Piece -> Char
pieceToCharUnicode (c, pt) =
  ( case c of
      White -> "♙♘♗♖♕♔"
      Black -> "♟♞♝♜♛♚"
  )
    !! fromEnum pt
type GameInfo = ((T.Text, T.Text), ZonedTime)
data ProgState = ProgState
  { psStockfish :: Maybe SfState
  , psGameExtra :: Maybe GameInfo
  }

data TuiEvent
  = TSfEvent SfState
  | TGameInfoEvent GameInfo

data Name
  = TuiBoard
  | TuiBoardExtra
  deriving (Eq, Ord)

renderRecord :: Record -> (Widget n, Widget n)
renderRecord
  Record
    { placement
    , activeColor
    , castling
    , enPassantTarget
    , halfMove
    , fullMove
    } = (uiBoard, uiExtra)
    where
      useUnicodeChar = False
      pToChar = if useUnicodeChar then pieceToCharUnicode else pieceToChar
      uiBoard =
        joinBorders $
          ( border
              ( hLimit 31 $
                  vBox (intersperse hBorder (fmap renderRank fenCoords))
              )
              <=> padLeft (Pad 1) auxRow
          )
            <+> padTop (Pad 1) auxCol
        where
          sp = vLimit 1 $ hLimit 3 $ str " "
          auxCol = hLimit 3 $ vBox $ intersperse sp $ fmap (mkSq . (: [])) ['8', '7' .. '1']
          auxRow = vLimit 1 $ hBox $ intersperse sp $ fmap (mkSq . (: [])) ['a' .. 'h']
          renderRank rankCoords =
            vLimit 1 $
              hBox $
                intersperse vBorder $
                  fmap (\coord -> mkSq [maybe ' ' pToChar $ at placement coord]) rankCoords
          mkSq n = joinBorders $ vLimit 1 $ hLimit 3 $ str $ " " <> n <> " "
      uiExtra =
        joinBorders $
          border $
            joinBorders $
              vLimit 1 $
                str
                  ( case activeColor of
                      White -> "w"
                      Black -> "b"
                  )
                  <+> vBorder
                  <+> str (show castling)
                  <+> vBorder
                  <+> str (maybe "-" show enPassantTarget)
                  <+> vBorder
                  <+> str (show halfMove)
                  <+> vBorder
                  <+> str (show fullMove)

ui ProgState {psStockfish, psGameExtra} =
  case psStockfish of
    Nothing -> initScreen
    Just SfState {sfPosition = Nothing} -> initScreen
    Just SfState {sfPosition = Just rc, sfStat = SfStat {ssNodes, ssNps, ssHashFull, ssTbHits}, sfPvs} ->
      let (board, extra) = renderRecord rc
          ac = activeColor rc
          renderPv (d, s, plies) = pprDep d <> " " <> pprScore ac s <> " " <> unwords (fmap pprPly $ take 6 plies)
          stats0 =
            str
              ( printf
                  "nodes: %s, nps: %s"
                  (maybe "-" show (coerce @_ @(Maybe Int) ssNodes))
                  (maybe "-" show (coerce @_ @(Maybe Int) ssNps))
              )
          stats1 =
            str
              ( printf
                  "hash: %s, tb: %s"
                  (maybe "-" show (coerce @_ @(Maybe Int) ssHashFull))
                  (maybe "-" show (coerce @_ @(Maybe Int) ssTbHits))
              )
          gameExtra = case psGameExtra of
            Nothing -> emptyWidget
            Just ((wn, bn), zt) ->
              let (wPrefix, bPrefix) = case ac of
                    White -> ("<□> ", " ■  ")
                    Black -> (" □  ", "<■> ")
               in str (bPrefix <> T.unpack bn)
                    <=> str (wPrefix <> T.unpack wn)
                    <=> str ("Last update: " <> show zt)
       in vCenter $
            vBox
              ( hCenter board :
                hCenter (padTop (Pad 1) extra) :
                hCenter gameExtra :
                hCenter stats0 :
                hCenter stats1 :
                fmap (hCenter . str . renderPv) sfPvs
              )
  where
    initScreen = center $ str "Initializing ..."

eventHandler = \case
  AppEvent (TSfEvent se) ->
    state \s -> ((), s {psStockfish = Just se})
  AppEvent (TGameInfoEvent v) ->
    state \s -> ((), s {psGameExtra = Just v})
  e ->
    resizeOrQuit e

mainWith :: Maybe Record -> Maybe String -> IO ()
mainWith mRc mEvent = do
  case (mRc, mEvent) of
    (Nothing, Nothing) -> die "both FEN and event path are empty."
    _ -> pure ()
  tz <- getCurrentTimeZone
  Just configFp <- lookupEnv "SXAKO_BRICK_CONFIG_PATH"
  cfg@Cfg.Config {} <- inputFile auto configFp
  (mIn, mqOut, sfWorker) <- start cfg
  case mRc of
    Just rc ->
      putMVar mIn (SfInFen rc Nothing)
    Nothing -> pure ()
  let app =
        App
          { appDraw = \r -> [ui r]
          , appHandleEvent = eventHandler
          , appChooseCursor = neverShowCursor
          , appStartEvent = pure ()
          , appAttrMap = \_ -> attrMap defAttr []
          }

  fin <- do
    eventChan <- Brick.BChan.newBChan 512

    case mEvent of
      Just eventPath -> do
        mgr <- newTlsManager
        gameFetcher <- async $ forever do
          resp <- Cb.getGameInfo mgr eventPath
          case resp of
            Left err -> die err
            Right (players, t, rawFen) -> do
              let pos = read @Record $ T.unpack rawFen
              putMVar mIn (SfInFen pos Nothing)
              let zt = utcToZonedTime tz t
              Brick.BChan.writeBChan eventChan $ TGameInfoEvent (players, zt)
          threadDelay 2_000_000
        pure ()
      Nothing -> pure ()

    _ <- async do
      forever do
        outs <- swapMVar mqOut mempty
        case outs of
          Seq.Empty -> pure ()
          _ Seq.:|> s -> Brick.BChan.writeBChan eventChan $ TSfEvent s
        threadDelay 200_000

    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    initialVty <- buildVty
    customMain @Name
      initialVty
      buildVty
      (Just eventChan)
      app
      (ProgState Nothing Nothing)
  pure ()

main :: IO ()
main = mainWith (Just initRecord) Nothing

mainWithArgs :: [String] -> IO ()
mainWithArgs = \case
  "fen" : args -> case args of
    [] -> mainWith (Just initRecord) Nothing
    [rawFen] -> mainWith (Just $ read rawFen) Nothing
    _ -> die "invalid args for `fen`"
  ["event", eventPath] -> mainWith Nothing (Just eventPath)
  [url]
    | Just ep <- stripPrefix "https://www.chess.com/events/" url -> do
      putStrLn $ "Redirecting with event path: " <> ep
      mainWithArgs ["event", ep]
  _ -> die "<prog> brick <fen> ..."
