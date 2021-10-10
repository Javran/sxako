{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.SanSpec where

import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Game.Sxako.Common
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.San
import Game.Sxako.TestData
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
  describe "legalSansEither" $
    describe "Right" $ do
      {-
        By verfiying Show instance on Sans, we can have coverage on
        whether check marks are set correctly.

        Note that some of the FENs below are also present in testdata/plies.yaml,
        which allows us to coverage on resulting Records, thus still necessary.
       -}
      describe "Sets of all SANs" $ do
        let mkTest tag rawFen sanWs =
              specify tag $ do
                let record = read @Record rawFen
                    Right result = legalSansEither record
                    allSanShows = S.fromList (show . fst <$> result)
                allSanShows `shouldBe` S.fromList (words sanWs)
        mkTest
          "Promotion with check"
          "6k1/4P2p/7K/8/8/8/8/8 w - - 0 1"
          "Kg5 Kh5 e8=B e8=N e8=Q# e8=R+"
        mkTest
          "Castle check"
          "3RnkbQ/4p1p1/8/8/8/8/8/4K2R w K - 1 1"
          "Kd1 Kd2 Ke2 Kf1 Kf2 O-O# \
          \Qh2 Qh3 Qh4 Qh5 Qh6 Qh7 Qxg7+ Qxg8+ \
          \Ra8 Rb8 Rc8 Rd1 Rd2 Rd3 Rd4 Rd5 Rd6 Rd7 Rf1# Rg1 Rh2 Rh3 Rh4 Rh5 Rh6 Rh7 Rxe8+"
        mkTest
          "Promotion disambiguation 0"
          "3n2k1/2PPP3/6K1/8/8/8/8/8 w - - 0 1"
          "Kf5 Kf6 Kg5 Kh5 Kh6 \
          \c8=B c8=N c8=Q c8=R \
          \cxd8=B cxd8=N cxd8=Q# cxd8=R# \
          \e8=B e8=N e8=Q# e8=R# \
          \exd8=B exd8=N exd8=Q# exd8=R#"
        mkTest
          "Promotion disambiguation 1"
          "3n2k1/3PP3/6K1/8/8/8/8/8 w - - 0 1"
          "Kf5 Kf6 Kg5 Kh5 Kh6 \
          \e8=B e8=N e8=Q# e8=R# \
          \exd8=B exd8=N exd8=Q# exd8=R#"
        mkTest
          "Non-promoting pawn capture needs disamb"
          "6k1/8/6K1/3n4/2P5/8/8/8 w - - 0 1"
          "Kf5 Kg5 Kh5 Kh6 c5 cxd5"

      describe "plies.yaml" $ do
        tds <- runIO $ loadTestDataList "testdata/plies.yaml"
        forM_ tds $ \TestData {tdTag, tdPosition, tdLegalPlies} -> do
          specify (T.unpack tdTag) $ case tdLegalPlies of
            Nothing -> pending
            Just expectedLps -> do
              let Right result = legalSansEither tdPosition
                  nextPositions = S.fromList (snd <$> result)
                  nextSans = S.fromList (fst <$> result)
              -- expect exact same set of resulting Records.
              nextPositions `shouldBe` S.fromList (M.elems expectedLps)
              -- expect that we have same # of Sans.
              S.size nextSans `shouldBe` M.size expectedLps
