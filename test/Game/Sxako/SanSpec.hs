{-# LANGUAGE OverloadedStrings #-}

module Game.Sxako.SanSpec where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Coord
import Game.Sxako.San
import Game.Sxako.Types
import Test.Hspec

spec :: Spec
spec = describe "sanP" $
  describe "examples" $ do
    let mkTest raw expected =
          specify raw $
            parseOnly sanP (BSC.pack raw)
              `shouldBe` Right expected

    mkTest "O-O" $
      SCastle KingSide Nothing
    mkTest "O-O#" $
      SCastle KingSide (Just Checkmate)
    mkTest "O-O-O+" $
      SCastle QueenSide (Just Check)
    mkTest "O-O-O#" $
      SCastle QueenSide (Just Checkmate)
    mkTest "e4" $
      SNorm Pawn Nothing False e4 Nothing Nothing
    mkTest "Bxh7" $
      SNorm Bishop Nothing True h7 Nothing Nothing
    mkTest "Nc5#" $
      SNorm Knight Nothing False c5 Nothing (Just Checkmate)
    mkTest "Qg2#" $
      SNorm Queen Nothing False g2 Nothing (Just Checkmate)
    mkTest "Kxe2" $
      SNorm King Nothing True e2 Nothing Nothing

    let [a, _b, _c, d, e, _f, _g, _h] = [0 .. 7]
    mkTest "Rdxb1" $
      SNorm Rook (Just (DisambByFile d)) True b1 Nothing Nothing
    mkTest "R2xb1+" $
      SNorm Rook (Just (DisambByRank (2 -1))) True b1 Nothing (Just Check)
    mkTest "Qg3xf4#" $
      SNorm Queen (Just (DisambByCoord g3)) True f4 Nothing (Just Checkmate)
    mkTest "exd5" $
      SNorm Pawn (Just (DisambByFile e)) True d5 Nothing Nothing
    mkTest "axb6#" $
      SNorm Pawn (Just (DisambByFile a)) True b6 Nothing (Just Checkmate)
