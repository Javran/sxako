{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.PgnSpec where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Pgn
import Test.Hspec

spec :: Spec
spec =
  describe "stringLitP" $ do
    let mkTest inp expected = do
          specify ("Example: " <> inp) $ do
            let r = parseOnly stringLitP (BSC.pack inp)
            r `shouldBe` Right expected

    mkTest "\"\"" ""
    mkTest "\"abcd\"" "abcd"
    mkTest "\"ab\\\\cd\"" "ab\\cd"
    mkTest "\"ab\\\\cd\\\"123\"" "ab\\cd\"123"

{-
  TODO: hspec-attoparsec
 -}
