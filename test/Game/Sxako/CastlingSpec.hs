{-# LANGUAGE TypeApplications #-}

module Game.Sxako.CastlingSpec where

import Control.Monad
import Data.Bits
import Game.Sxako.Castling
import Game.Sxako.Common
import Test.Hspec

spec :: Spec
spec = do
  describe "Castling" $ do
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
  describe "minusCastleRight" $ do
    let mkExample (i, (a, b, c)) =
          specify ("example #" <> show i) $
            (read a `minusCastleRight` read b) `shouldBe` (read c)
    mapM_ mkExample $
      zip
        [0 :: Int ..]
        -- (a,b,c): expect a `minus` b === c.
        [ ("KQ", "K", "Q")
        , ("KQ", "k", "KQ")
        , ("KQkq", "K", "Qkq")
        , ("kq", "KQ", "kq")
        , ("KQkq", "Qq", "Kk")
        ]
  describe "removeCastleRight" $ do
    let mkExample (i, (rawA, ew, eb)) =
          specify ("example #" <> show i) $ do
            let a = read rawA
            removeCastleRight White a `shouldBe` read ew
            removeCastleRight Black a `shouldBe` read eb
    mapM_ mkExample $
      zip
        [0 :: Int ..]
        -- (a,ew,eb): expect a minus White to be ew, minus Black to be eb.
        [ ("KQ", "", "KQ")
        , ("KQkq", "kq", "KQ")
        , ("Kq", "q", "K")
        , ("Qkq", "kq", "Q")
        ]
