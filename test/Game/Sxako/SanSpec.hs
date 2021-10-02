{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.SanSpec where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.San
import Game.Sxako.Types
import Game.Sxako.Coord
import Test.Hspec

spec :: Spec
spec = describe "sanP" $
  describe "castleP" $
    describe "examples" $ do
      let mkTest raw expected =
            specify raw $
              parseOnly sanP (BSC.pack raw)
                `shouldBe` Right expected

      mkTest "O-O" $ SCastle KingSide Nothing
      mkTest "O-O#" $ SCastle KingSide (Just Checkmate)
      mkTest "O-O-O+" $ SCastle QueenSide (Just Check)
      mkTest "O-O-O#" $ SCastle QueenSide (Just Checkmate)
      mkTest "e4" $ SNorm Pawn Nothing False e4 Nothing Nothing
      mkTest "Bxh7" $ SNorm Bishop Nothing True h7 Nothing Nothing
      mkTest "Nc5#" $ SNorm Knight Nothing False c5 Nothing (Just Checkmate)
      mkTest "Qg2#" $ SNorm Queen Nothing False g2 Nothing (Just Checkmate)
      mkTest "Kxe2" $ SNorm King Nothing True e2 Nothing Nothing
