module Game.Sxako.Cli.Tablebase where

import GHC.Generics

import Data.Aeson
import qualified Data.Text as T

-- reference: https://github.com/lichess-org/lila-tablebase
data TbResult = TbResult
  { dtz :: Maybe Int
  , dtm :: Maybe Int
  , checkmate :: Bool
  , stalemate :: Bool
  , insufficient_material :: Bool
  , category :: T.Text
  , moves :: [Value]
  }
  deriving (Generic, Show)

instance FromJSON TbResult
