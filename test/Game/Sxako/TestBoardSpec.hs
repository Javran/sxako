{-# LANGUAGE TypeApplications #-}
module Game.Sxako.TestBoardSpec where

import Game.Sxako.TestBoard
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Monad
import Data.List

spec :: Spec
spec = describe "TestBoard" $
  prop "Read & Show instance" $ do
   let genSquare = elements "_PpNnBbRrQqKk"
       genRank = replicateM 8 genSquare
       genRawBoard = intercalate "|" <$> replicateM 8 genRank
   rawBd <- genRawBoard
   pure $ show (read @TestBoard rawBd) === rawBd
