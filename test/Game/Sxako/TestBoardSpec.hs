{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.TestBoardSpec where

import Control.Monad
import Data.Attoparsec.ByteString.Char8 as Parser
import Data.List
import Game.Sxako.Fen
import Game.Sxako.TestBoard
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = describe "TestBoard" $ do
  specify "example" $ do
    let Right record =
          parseOnly fenP "2r1k2r/3p1p2/pq2p2p/4P1p1/2P2n2/3Q1PB1/2P2P1P/R4RK1 w k - 5 23"
    read @TestBoard
      "__r_k__r|\
      \___p_p__|\
      \pq__p__p|\
      \____P_p_|\
      \__P__n__|\
      \___Q_PB_|\
      \__P__P_P|\
      \R____RK_"
      `shouldBe` TestBoard (placement record)
  prop "Read & Show instance" $ do
    let genSquare = elements "_PpNnBbRrQqKk"
        genRank = replicateM 8 genSquare
        genRawBoard = intercalate "|" <$> replicateM 8 genRank
    rawBd <- genRawBoard
    pure $ show (read @TestBoard rawBd) === rawBd
