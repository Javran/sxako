{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Game.Sxako.PgnSpec where

import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Pgn
import Test.Hspec
import Test.Hspec.Attoparsec
import Text.RawString.QQ

spec :: Spec
spec =
  describe "stringLitP" $
    describe "examples" $ do
      let mkTest inp expected =
            specify inp $
              BSC.pack inp ~> stringLitP
                `parseSatisfies` (== expected)

      mkTest [r|""|] [r||]
      mkTest [r|"abcd"|] [r|abcd|]
      mkTest [r|"ab\\cd"|] [r|ab\cd|]
      mkTest [r|"ab\\cd\"\\123"|] [r|ab\cd"\123|]
