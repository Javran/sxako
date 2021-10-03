{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.TestData where

import Data.Aeson
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Game.Sxako.Fen
import Game.Sxako.Ply

data TestData = TestData
  { tdTag :: T.Text
  , tdPosition :: Record
  , tdLegalPlies :: Maybe (M.Map Ply Record)
  }
  deriving (Show)

instance FromJSON TestData where
  parseJSON = withObject "TestData" $ \o -> do
    tdTag <- o .: "tag"
    tdPosition <- o .: "position"
    tdLegalPlies <- o .:? "legal-plies"
    pure $ TestData {tdTag, tdPosition, tdLegalPlies}

instance ToJSON TestData where
  toJSON TestData {tdTag, tdPosition, tdLegalPlies} =
    object $
      [ "tag" .= tdTag
      , "position" .= tdPosition
      ]
        <> maybe [] ((: []) . ("legal-plies" .=)) tdLegalPlies

  toEncoding TestData {tdTag, tdPosition, tdLegalPlies} =
    pairs
      ("tag" .= tdTag
         <> "position" .= tdPosition
         <> maybe mempty ("legal-plies" .=) tdLegalPlies)
