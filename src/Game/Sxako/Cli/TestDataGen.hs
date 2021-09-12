{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications, OverloadedStrings #-}

module Game.Sxako.Cli.TestDataGen where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml
import Game.Sxako.Fen
import System.Environment
import System.Exit

type Ply = T.Text -- TODO: use proper type

data TestPayload
  = -- | all legal moves and their resulting positions should match.
    LegalPlies (Maybe (M.Map T.Text Record))
  | -- | follow along a game and expect resulting positions to match
    Follow (Maybe [(T.Text, Record)])
    deriving (Show)

data TestData = TestData
  { tdTag :: T.Text
  , tdPosition :: Record
  , tdPayload :: TestPayload
  } deriving (Show)

instance FromJSON TestData where
  parseJSON = withObject "TestData" $ \o -> do
    tdTag <- o .: "tag"
    tdPosition <- o .: "position"
    mode <- o.: "mode"
    tdPayload <- if
      | mode == "legal-plies" -> pure (LegalPlies Nothing)
      | mode == "follow" -> pure (Follow Nothing)
      | otherwise -> fail $ "Unknown mode: " <> mode
    pure $ TestData {tdTag, tdPosition, tdPayload}

subCmdMain :: String -> IO ()
subCmdMain cmdHelpPrefix =
  getArgs >>= \case
    [inputFp] -> do
      r <- decodeFileEither @[TestData] inputFp
      print r
    _ -> do
      putStrLn $ cmdHelpPrefix <> "<testdata>"
      exitFailure
