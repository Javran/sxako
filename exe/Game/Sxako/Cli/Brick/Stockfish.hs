module Game.Sxako.Cli.Brick.Stockfish
  ( SfStat (..)
  , SfState (..)
  , SfIn (..)
  , start
  )
where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Data.Char
import qualified Data.DList as DL
import Data.Either
import Data.List
import qualified Data.List.HT
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Game.Sxako.Cli.Brick.Config as Cfg
import Game.Sxako.Cli.Brick.Stockfish.Types
import System.Exit
import System.IO
import System.Process
import Text.ParserCombinators.ReadP hiding (get)

type SfOut = SfState

toSfStatPart :: InfoComp -> SfStat
toSfStatPart = \case
  Nodes v -> mempty {ssNodes = Last (Just v)}
  Nps v -> mempty {ssNps = Last (Just v)}
  HashFull v -> mempty {ssHashFull = Last (Just v)}
  TbHits v -> mempty {ssTbHits = Last (Just v)}
  CpuLoad v -> mempty {ssCpuLoad = Last (Just v)}
  Time v -> mempty {ssTime = Last (Just v)}
  _ -> mempty

separatePvInfo :: [InfoComp] -> Maybe (Int, PvInfo, [InfoComp])
separatePvInfo xs0 = do
  (Pv pvs, xs1) <-
    rm
      ( \case
          Pv {} -> True
          _ -> False
      )
      xs0
  (Score s, xs2) <-
    rm
      ( \case
          Score {} -> True
          _ -> False
      )
      xs1
  (Depth d, xs3) <-
    rm
      ( \case
          Depth {} -> True
          _ -> False
      )
      xs2
  (mSd, xs4) <-
    ( do
        (SelDepth sd, b) <-
          rm
            ( \case
                SelDepth {} -> True
                _ -> False
            )
            xs3
        pure (Just sd, b)
      )
      <|> pure (Nothing, xs3)
  (MultiPv m, xs5) <-
    rm
      ( \case
          MultiPv {} -> True
          _ -> False
      )
      xs4
      <|> Just (MultiPv 1, xs4)
  pure (m, ((d, mSd), s, pvs), xs5)
  where
    rm pre xs = do
      (ls, v : rs) <- pure (break pre xs)
      pure (v, ls <> rs)

infoLineP :: ReadP [InfoComp]
infoLineP = do
  _ <- string "info "
  infoCompP `sepBy1` char ' '
  where
    decimal1 = read <$> munch1 isDigit
    signedDecimal1 = do
      c <-
        ((-1) <$ char '-')
          <++ (1 <$ char '+')
          <++ pure 1
      v <- decimal1
      pure $ v * c

    p prefix constr valP = do
      _ <- string prefix
      _ <- char ' '
      constr <$> valP

    pInt pre c = p pre c decimal1

    coordP = do
      f <- satisfy (\ch -> ch >= 'a' && ch <= 'h')
      r <- satisfy (\ch -> ch >= '1' && ch <= '8')
      pure [f, r]
    plyP = do
      src <- coordP
      dst <- coordP
      pm <- (Just <$> satisfy (`elem` ("qrbn" :: [] Char))) <++ pure Nothing
      pure (src, dst, pm)

    scoreP = do
      let cpP = CentiPawn <$> (string "cp " *> signedDecimal1)
          mateP = Mate <$> (string "mate " *> signedDecimal1)
      sp <- cpP <++ mateP
      b <-
        (Just ScLowerbound <$ string " lowerbound")
          <++ (Just ScUpperbound <$ string " upperbound")
          <++ pure Nothing
      pure (sp, b)

    infoCompP :: ReadP InfoComp
    infoCompP =
      pInt "depth" Depth
        <++ pInt "seldepth" SelDepth
        <++ pInt "time" Time
        <++ pInt "nodes" Nodes
        <++ p "pv" Pv (plyP `sepBy1` char ' ')
        <++ pInt "multipv" MultiPv
        <++ p "score" Score scoreP
        <++ p "currmove" CurrMove plyP
        <++ pInt "currmovenumber" CurrMoveNumber
        <++ pInt "hashfull" HashFull
        <++ pInt "nps" Nps
        <++ pInt "tbhits" TbHits
        <++ pInt "cpuload" CpuLoad
        <++ p "string" InfoStr (munch1 (const True))

start ::
  Cfg.Config ->
  IO
    ( MVar SfIn
    , MVar (Seq.Seq SfOut)
    , Async (ExitCode, SfState)
    )
start Cfg.Config {Cfg.stockfishBin, Cfg.multiPv, Cfg.hash, Cfg.threads, Cfg.syzygyPath} = do
  let cp =
        (proc stockfishBin [])
          { std_in = CreatePipe
          , std_out = CreatePipe
          , std_err = Inherit
          }
  (Just hIn, Just hOut, _, ph) <- createProcess cp
  hSetBuffering hIn LineBuffering

  let tbPath = intercalate ":" syzygyPath

  mapM_
    (hPutStrLn hIn)
    [ "uci"
    , "setoption name Hash value " <> show hash
    , "setoption name MultiPV value " <> show multiPv
    , "setoption name Threads value " <> show threads
    , "setoption name SyzygyPath value " <> tbPath
    , "isready"
    ]

  -- TODO: probably we can print & parse this part in a better way
  fix \loop -> do
    resp <- hGetLine hOut
    case resp of
      "readyok" -> putStrLn "ready"
      r -> do
        putStrLn $ "SF: " <> r
        loop

  -- input and output message queue to this worker.
  mIn <- newEmptyMVar @SfIn
  mqOut <- newMVar @(Seq.Seq SfOut) mempty
  let worker = do
        -- process incoming message first.
        mi <- liftIO $ tryTakeMVar mIn
        expectQuit <- case mi of
          Nothing -> pure False
          Just cmd ->
            case cmd of
              SfInQuit ->
                True <$ liftIO (hPutStrLn hIn "quit")
              SfInFen fen e -> do
                oldPos <- gets sfPosition
                needUpdate <-
                  case oldPos of
                    Nothing -> pure True
                    Just p' | p' == fen -> pure False
                    Just _ ->
                      True
                        <$
                        -- TODO: should we issue and wait for a 'readyok' here?
                        liftIO do
                          hPutStrLn hIn "stop"
                when needUpdate do
                  liftIO do
                    hPutStrLn hIn $ "position fen " <> show fen
                    hPutStrLn hIn "go infinite"
                  modify (\s -> s {sfPosition = Just fen, sfGameExtra = e})
                pure False
        -- TODO: process stockfish output here.
        rawOuts <-
          fix
            ( \loop outs -> do
                hasInp <- liftIO do hWaitForInput hOut 20
                if hasInp
                  then do
                    r <- liftIO do hGetLine hOut
                    loop (outs <> DL.singleton r)
                  else do
                    -- send an empty command to keep things alive, not sure why I have to do this, but it seems to work.
                    unless expectQuit do
                      liftIO do hPutStrLn hIn ""
                    pure outs
            )
            DL.empty
        -- have a round of groupping here to group MultiPVs.
        let sfParsed raw = case readP_to_S (infoLineP <* eof) raw of
              [(v, "")] -> Right v
              _ -> Left raw
            (_ls, outs0) =
              -- TODO: report `_ls` somehow?
              partitionEithers (fmap sfParsed (DL.toList rawOuts))
            outs1 :: [Either [InfoComp] [(PvInfo, [InfoComp])]]
            outs1 =
              fmap cleanup
                . Data.List.HT.groupBy (curry shouldMerge)
                . fmap (\v -> maybe (Left v) Right (separatePvInfo v))
                $ outs0
              where
                shouldMerge = \case
                  (Right (a, _, _), Right (b, _, _)) | a + 1 == b -> True
                  (_, _) -> False
                cleanup = \case
                  [Left v] -> Left v
                  rsPre@(_ : _) ->
                    let rs = fmap ((\(_, x, y) -> (x, y)) . fromRight (error "unexpected Left")) rsPre
                     in Right rs
                  _ -> error "unexpected: []"
            statUpd :: SfStat
            mPvInfo :: Last [PvInfo]
            (statUpd, mPvInfo) =
              foldMap
                ( \case
                    Left xs -> (foldMap toSfStatPart xs, mempty)
                    Right xs -> (foldMap (foldMap toSfStatPart . snd) xs, Last (Just $ fmap fst xs))
                )
                outs1

        modify
          ( \(SfState s pvs pos extra) ->
              let Last (Just newPvs) = Last (Just pvs) <> mPvInfo
               in SfState (s <> statUpd) newPvs pos extra
          )
        s <- get
        liftIO $ modifyMVar_ mqOut (\q -> pure $ q <> Seq.singleton s)
        if expectQuit
          then liftIO do
            hClose hIn
            hClose hOut
            waitForProcess ph
          else worker

  aWorker <- async (runStateT worker (SfState mempty [] Nothing Nothing))
  pure (mIn, mqOut, aWorker)
