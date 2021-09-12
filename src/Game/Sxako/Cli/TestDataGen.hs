{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.Cli.TestDataGen where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Yaml
import Game.Sxako.Fen
import Game.Sxako.Move
import System.Environment
import System.Exit

data TestPayload
  = -- | all legal moves and their resulting positions should match.
    LegalPlies (Maybe (M.Map Ply Record))
  | -- | follow along a game and expect resulting positions to match
    Follow (Maybe [(Ply, Record)])
  deriving (Show)

data TestData = TestData
  { tdTag :: T.Text
  , tdPosition :: Record
  , tdPayload :: TestPayload
  }
  deriving (Show)

instance FromJSON TestData where
  parseJSON = withObject "TestData" $ \o -> do
    tdTag <- o .: "tag"
    tdPosition <- o .: "position"
    mode <- o .: "mode"
    tdPayload <-
      if
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

stockfishSetup :: IO ()
stockfishSetup = pure () -- TODO
