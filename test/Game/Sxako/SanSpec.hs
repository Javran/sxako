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
  Generates random SANs for tests.

  Note that this might generate non-sensical moves,
  but it should be good enough for the purpose of verifying
  Read & Show instance.
 -}
genSan :: Gen San
genSan =
  frequency
    [ (9, genNorm)
    , (1, genCastle)
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
    prop "Read & Show instance" $ do
      s <- genSan
      let lbl = case s of
            SNorm {} -> "SNorm"
            SCastle {} -> "SCastle"
      pure $ label lbl $ read (show s) === s

{-
  TODO: test coverage for Ply to San conversion:

  Here we trust that Game.Sxako.Ply has been implemented correctly,
  and verify the correctness of legalSansEither.
  In particular:

  - Given same input (we assume that the WHNF of the result is always Right),
    legalPliesEither and legalSansEither should:

    + have the exact same Set of Records.
    + # of Ply in the Set should be the same as that of San
      (i.e. disambiguation does resolve conflicts)

  - also a few sample FENs for testing:

    + kn6/r3r3/1n6/2Q1Q3/8/2Q1Q3/8/7K w - - 0 1
    + kn6/r3r3/1n6/2Q1Q3/8/2Q1Q3/8/7K b - - 0 1


 -}
