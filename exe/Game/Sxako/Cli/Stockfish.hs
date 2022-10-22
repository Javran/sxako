module Game.Sxako.Cli.Stockfish (
  SfProcess,
  startStockfish,
  stopStockfish,
  withStockfish,
  getAllLegalPlies,
  getNextPosition,
) where

{-
  Communication with stockfish through UCI protocol and stockfish-specific commands.

  Reference: http://wbec-ridderkerk.nl/html/UCIProtocol.html

  Note that here we are using stockfish just to see all legal moves given a FEN string.
  So this module sets up MultiPV to its maximum value and assume we always
  go with depth=1 all the time.

 -}
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Monoid
import Game.Sxako.Fen
import Game.Sxako.Ply
import System.Exit
import System.IO
import System.Process.Typed

newtype SfProcess = SfProcess (Process Handle Handle ())

startStockfish :: IO SfProcess
startStockfish = do
  let pc =
        setStdout createPipe
          . setStdin createPipe
          $ proc "stockfish" []
  p <- startProcess pc
  let hIn = getStdin p
      hOut = getStdout p
      collectOptions :: Maybe Int -> IO (Maybe Int)
      collectOptions m = do
        -- get UCI options, for now we are only interested in max of MultiPV.
        raw <- hGetLine hOut
        if
            | raw == "uciok" -> pure m
            | "option name MultiPV" `isPrefixOf` raw ->
              collectOptions
                (m <|> (Just . read . last . words $ raw))
            | otherwise -> collectOptions m

  hSetBuffering hIn LineBuffering
  _welcomeMsg <- hGetLine hOut
  hPutStrLn hIn "uci"
  Just c <- collectOptions Nothing
  hPutStrLn hIn $ "setoption name MultiPV value " <> show c
  hPutStrLn hIn "isready"
  do
    raw <- hGetLine hOut
    guard $ raw == "readyok"
  pure $ SfProcess p

stopStockfish :: SfProcess -> IO ExitCode
stopStockfish (SfProcess p) = do
  let hIn = getStdin p
  hPutStrLn hIn "quit"
  waitExitCode p

withStockfish :: (SfProcess -> IO r) -> IO r
withStockfish f = runResourceT $ do
  (k, sf) <- allocate startStockfish (void . stopStockfish)
  r <- liftIO $ f sf
  r <$ release k

type PartialInfo = (Last Int, Last Ply)

consumePartialInfo :: [String] -> Either String (PartialInfo, [String])
consumePartialInfo = \case
  "depth" : _ : xs -> pure (mempty, xs)
  "seldepth" : _ : xs -> pure (mempty, xs)
  "time" : _ : xs -> pure (mempty, xs)
  "nodes" : _ : xs -> pure (mempty, xs)
  "pv" : raw : _xs | p <- read @Ply raw -> pure ((mempty, Last (Just p)), [])
  "multipv" : raw : xs | v <- read @Int raw -> pure ((Last (Just v), mempty), xs)
  "score" : _ : _ : xs -> pure (mempty, xs)
  "currmove" : _ : xs -> pure (mempty, xs)
  "currmovenumber" : _ : xs -> pure (mempty, xs)
  "hashfull" : _ : xs -> pure (mempty, xs)
  "nps" : _ : xs -> pure (mempty, xs)
  "tbhits" : _ : xs -> pure (mempty, xs)
  "cpuload" : _ : xs -> pure (mempty, xs)
  "string" : _ -> pure (mempty, [])
  "refutation" : _ -> pure (mempty, [])
  "currline" : _ -> pure (mempty, [])
  unknownTok : _ -> Left unknownTok
  _ -> pure (mempty, [])

parseInfo :: [String] -> Either String PartialInfo
parseInfo = \case
  [] -> pure mempty
  xs@(_ : _) -> do
    (r, ys) <- consumePartialInfo xs
    (r <>) <$> parseInfo ys

getCurrentPosition :: SfProcess -> IO Record
getCurrentPosition (SfProcess p) = do
  let hIn = getStdin p
      hOut = getStdout p
  hPutStrLn hIn "d"
  Just endFenRaw <-
    fix
      ( \loop cur -> do
          raw <- hGetLine hOut
          let cur' =
                cur <|> do
                  guard $ "Fen: " `isPrefixOf` raw
                  Just (drop 5 raw)
          -- very ugly way of checking end of the response, but I don't have anything better.
          if "Checkers: " `isPrefixOf` raw
            then pure cur'
            else loop cur'
      )
      Nothing
  let sfRecord = read @Record endFenRaw
  pure sfRecord

getNextPosition :: SfProcess -> Record -> Ply -> IO Record
getNextPosition sf@(SfProcess p) pos ply = do
  let hIn = getStdin p
  hPutStrLn hIn $ "position fen " <> show pos <> " moves " <> show ply
  getCurrentPosition sf

getAllLegalPlies :: SfProcess -> Record -> IO (M.Map Ply Record)
getAllLegalPlies sf@(SfProcess p) pos = do
  let hIn = getStdin p
      hOut = getStdout p
  hPutStrLn hIn $ "position fen " <> show pos
  hPutStrLn hIn "go depth 1"
  let collectInfo :: IO [[] String]
      collectInfo = do
        raw <- hGetLine hOut
        let ws = words raw
        if
            | "bestmove" `isPrefixOf` raw -> pure []
            | ["info", "string"] `isPrefixOf` ws -> collectInfo
            | ["info", "depth", "0", "score", "mate", "0"] == ws ->
              -- TODO: quick and dirty hack, might need to do something better than this.
              collectInfo
            | ["info"] `isPrefixOf` ws -> (tail ws :) <$> collectInfo
            | otherwise -> collectInfo

      dbgParse :: [String] -> IO (Int, Ply)
      dbgParse xs = case parseInfo xs of
        Left msg -> do
          putStrLn $ "Parse failed for input: " <> show xs
          putStrLn $ "Reason: " <> msg
          exitFailure
        Right m@(Last ma, Last mb) ->
          case (,) <$> ma <*> mb of
            Nothing -> do
              putStrLn $ "Parse failed, only collected: " <> show m
              putStrLn $ "Input was: " <> show xs
              exitFailure
            Just v -> pure v
  sfLegalPlies <- collectInfo >>= mapM dbgParse
  sfPairs <- forM sfLegalPlies $ \(_pvId, ply) -> do
    sfRecord <- getNextPosition sf pos ply
    pure (ply, sfRecord)
  pure . M.fromList $ sfPairs
