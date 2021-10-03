module Game.Sxako.CommonSpec where

import Control.Monad
import Game.Sxako.Common
import Test.Hspec

spec :: Spec
spec = describe "charToPiece & pieceToChar" $
  specify "charToPiece . pieceToChar === Just" $
    forM_ [(c, pt) | c <- universe, pt <- universe] $ \p ->
      (charToPiece . pieceToChar) p `shouldBe` Just p
