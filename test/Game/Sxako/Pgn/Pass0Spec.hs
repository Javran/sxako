{-# LANGUAGE QuasiQuotes #-}

module Game.Sxako.Pgn.Pass0Spec where

import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Common
import Game.Sxako.Pgn.Pass0
import Test.Hspec
import Test.Hspec.Attoparsec
import Text.RawString.QQ

spec :: Spec
spec = do
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
  describe "sanSuffixP" $
    describe "examples" $ do
      let mkTest inp expected =
            specify inp $
              BSC.pack inp ~> sanSuffixP
                `parseSatisfies` (== expected)
      mkTest "!" 1
      mkTest "?" 2
      mkTest "!!" 3
      mkTest "??" 4
      mkTest "!?" 5
      mkTest "?!" 6
  describe "movetextResultP" $
    describe "examples" $ do
      let mkTest inp expected =
            specify inp $
              BSC.pack inp ~> movetextResultP
                `shouldParse` expected
      mkTest "1-0" $ MtrWon White
      mkTest "0-1" $ MtrWon Black
      mkTest "1/2-1/2" MtrDrawn
      mkTest "*" MtrUnknown
