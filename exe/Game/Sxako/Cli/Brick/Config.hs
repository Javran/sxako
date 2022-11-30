module Game.Sxako.Cli.Brick.Config where

import Dhall

data Config = Config
  { multiPv :: Natural
  , hash :: Natural
  , threads :: Natural
  , syzygyPath :: [FilePath]
  , stockfishBin :: FilePath
  }
  deriving (Generic)

instance FromDhall Config
