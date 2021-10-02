{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.SanSpec where

import Data.Attoparsec.ByteString.Char8 as Parser
import Game.Sxako.San
import Game.Sxako.Types
import Test.Hspec

spec :: Spec
spec = describe "sanP" $
  describe "castleP" $
    specify "examples" $ do
      parseOnly sanP "O-O"
        `shouldBe` Right (SCastle KingSide Nothing)
      parseOnly sanP "O-O#"
        `shouldBe` Right (SCastle KingSide (Just Checkmate))
      parseOnly sanP "O-O-O+"
        `shouldBe` Right (SCastle QueenSide (Just Check))
      parseOnly sanP "O-O-O#"
        `shouldBe` Right (SCastle QueenSide (Just Checkmate))
