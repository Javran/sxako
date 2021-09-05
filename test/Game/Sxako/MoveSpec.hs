module Game.Sxako.MoveSpec where

import Game.Sxako.Move
import Game.Sxako.TestBoard
import Game.Sxako.Types
import Test.Hspec

{-
  TODO: test coverage:

  - rooks
  - queens
  - kings

  - long-range pieces should be blocked properly

 -}
spec :: Spec
spec = describe "attackingSquares" $ do
  let mkTest rawBd rawExpectWhite rawExpectBlack = do
        let [(TestBoard bd, "")] = reads rawBd
        specify "White" $ do
          let [(eW, "")] = reads rawExpectWhite
          attackingSquares bd White `shouldBe` eW
        specify "Black" $ do
          let [(eB, "")] = reads rawExpectBlack
          attackingSquares bd Black `shouldBe` eB
  describe "empty board" $
    -- This is provided mostly for copy-pasting new test cases.
    mkTest
      "________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________"
      -- White
      "________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________"
      -- Black
      "________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________"
  describe "pawns" $
    mkTest
      "________|\
      \_p_____p|\
      \________|\
      \________|\
      \____P__P|\
      \________|\
      \________|\
      \________"
      -- White
      "________|\
      \________|\
      \________|\
      \___*_**_|\
      \________|\
      \________|\
      \________|\
      \________"
      -- Black
      "________|\
      \________|\
      \*_*___*_|\
      \________|\
      \________|\
      \________|\
      \________|\
      \________"
  describe "knights" $
    mkTest
      "_n______|\
      \________|\
      \________|\
      \____N___|\
      \____n___|\
      \________|\
      \________|\
      \_______N"
      -- White
      "________|\
      \___*_*__|\
      \__*___*_|\
      \________|\
      \__*___*_|\
      \___*_**_|\
      \_____*__|\
      \________"
      -- Black
      "________|\
      \___*____|\
      \*_**_*__|\
      \__*___*_|\
      \________|\
      \__*___*_|\
      \___*_*__|\
      \________"
  describe "bishops" $
    mkTest
      "________|\
      \________|\
      \__BB_b__|\
      \________|\
      \____b___|\
      \________|\
      \________|\
      \________"
      -- White
      "**__**__|\
      \_****___|\
      \________|\
      \_****___|\
      \**__**__|\
      \*_____*_|\
      \_______*|\
      \________"
      -- Black
      "___*___*|\
      \____*_**|\
      \__*___*_|\
      \___****_|\
      \___*___*|\
      \__**_*__|\
      \_**___*_|\
      \**_____*"
