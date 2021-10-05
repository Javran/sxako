{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.SanSpec where

import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.San
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

{-
  A newtype meant for generating random SAN for tests.

  The Arbitrary instance might generate non-sensical moves,
  but it should be good enough for the purpose of verifying
  Read & Show instance.
 -}
newtype GenSan = GenSan San deriving (Show)

instance Arbitrary GenSan where
  arbitrary =
    frequency
      [ (9, GenSan <$> genNorm)
      , (1, GenSan <$> genCastle)
      ]
    where
      gen07 = chooseInt (0, 7)
      genCheck = elements [Nothing, Just Check, Just Checkmate]
      genCoord = unsafeFromRankAndFile <$> gen07 <*> gen07
      genNorm =
        SNorm <$> elements @PieceType universe
          <*> oneof
            (pure Nothing :
             (fmap . fmap)
               Just
               [ DisambByFile <$> gen07
               , DisambByRank <$> gen07
               , DisambByCoord <$> genCoord
               ])
          <*> elements [False, True]
          <*> genCoord
          <*> elements (Nothing : fmap Just [Knight .. Queen])
          <*> genCheck
      genSide = elements [KingSide, QueenSide]
      genCastle = SCastle <$> genSide <*> genCheck

spec :: Spec
spec = do
  describe "sanP" $
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

      let [a, _b, _c, d, e, _f, g, _h] = [0 .. 7]
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
      mkTest "gxh1=Q+" $
        SNorm
          { sPieceFrom = Pawn
          , sFrom = Just (DisambByFile g)
          , sCapture = True
          , sTo = h1
          , sPromo = Just Queen
          , sCheck = Just Check
          }
  describe "San" $
    prop "Read & Show instance" $
      \(GenSan s) ->
        read (show s) === s
