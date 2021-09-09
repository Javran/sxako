module Game.Sxako.CastlingSpec where

import Control.Monad
import Data.Bits
import Game.Sxako.Castling
import Game.Sxako.Types
import Test.Hspec

spec :: Spec
spec = describe "Castling" $ do
  specify "examples" $ do
    read "-" `shouldBe` none
    read "KQkq" `shouldBe` allAllowed
    read "Kq" `shouldBe` (whiteKingSide .|. blackQueenSide)
    {-
      FEN spec requires chars to have a specific order,
      here we check the parser consumes as much as it could, and have some leftover.
     -}
    reads @Castling "QqK" `shouldBe` [(whiteQueenSide .|. blackQueenSide, "K")]

  specify "Read & Show instance" $
    -- possible set of values are small, might as well test all of them
    forM_ (universe @Castling) $ \c ->
      read (show c) `shouldBe` c
