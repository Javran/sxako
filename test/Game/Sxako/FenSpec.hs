{-# LANGUAGE NamedFieldPuns #-}
module Game.Sxako.FenSpec where

import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import qualified Data.ByteString.Char8 as BSC
import Game.Sxako.Fen
import Test.Hspec

{-
  Puzzles are randomly picked from: https://database.lichess.org/#puzzles
 -}

spec :: Spec
spec = describe "fenP" $ do
  describe "samples" $ do
    let rawFens =
          [ ( "standard initial board"
            , "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            )
          , ( "Lichess wF4aq"
            , "2r1k2r/3p1p2/pq2p2p/4P1p1/2P2n2/3Q1PB1/2P2P1P/R4RK1 w k - 5 23"
            )
          , ( "Lichess A5YpK"
            , "6k1/pp2nrbp/3pQ3/8/3pP2P/8/PPP1qPP1/2KRR3 b - - 2 21"
            )
          , ( "Lichess vScGn"
            , "1r6/r2k4/2Rppp2/1p1P2p1/1P2PnPp/pK3P1P/P7/2R2B2 w - - 0 43"
            )
          , ( "Lichess EwGV4"
            , "RQ3bk1/5pp1/p6p/1p6/3P4/q3P1P1/5PKP/8 w - - 0 31"
            )
          , ( "Lichess N3c34"
            , "b5r1/p1pk2pp/8/2b1P3/4n3/2P5/PP4PP/RNB2R1K w - - 3 20"
            )
          ]
    forM_ rawFens $ \(tag, rawInp) -> do
      specify tag $
        case parseOnly fenP (BSC.pack rawInp) of
          Left err -> fail err
          Right Record {placement = bd, halfMove, fullMove} -> do
            let [ expected
                  , _activeColor
                  , _castling
                  , _enPassant
                  , halfMoveRaw
                  , fullMoveRaw
                  ] = words rawInp
            {-
              This relys on the fact that Show instance of a Board
              is the same as Fen placement notation.
             -}
            show bd `shouldBe` expected
            {-
              Test that two Ints match repsectively.
             -}
            halfMove `shouldBe` read halfMoveRaw
            fullMove `shouldBe` read fullMoveRaw
