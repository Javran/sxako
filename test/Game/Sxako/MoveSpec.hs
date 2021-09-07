module Game.Sxako.MoveSpec where

import qualified Data.Map.Strict as M
import Data.Maybe
import Game.Sxako.Castling
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Move
import Game.Sxako.TestBoard
import Game.Sxako.Types
import Test.Hspec

spec :: Spec
spec = do
  attackingSquaresSpec
  pawnPliesSpec

attackingSquaresSpec :: Spec
attackingSquaresSpec = describe "attackingSquares" $ do
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
  describe "rooks" $
    mkTest
      "________|\
      \__r___R_|\
      \________|\
      \________|\
      \___r____|\
      \________|\
      \____R___|\
      \________"
      -- White
      "____*_*_|\
      \__****_*|\
      \____*_*_|\
      \____*_*_|\
      \____*_*_|\
      \____*_*_|\
      \****_***|\
      \____*_*_"
      -- Black
      "__**____|\
      \**_****_|\
      \__**____|\
      \__**____|\
      \***_****|\
      \__**____|\
      \__**____|\
      \__**____"
  describe "queens" $
    mkTest
      "________|\
      \_Q______|\
      \____q___|\
      \______q_|\
      \________|\
      \________|\
      \___Q____|\
      \________"
      -- White
      "****____|\
      \*_******|\
      \****____|\
      \**_*__*_|\
      \_*_***__|\
      \_*****__|\
      \***_****|\
      \_****__*"
      -- Black
      "__***_*_|\
      \___****_|\
      \****_***|\
      \******_*|\
      \__*_****|\
      \_*__*_**|\
      \*__**_*_|\
      \____*_*_"
  describe "kings" $
    mkTest
      "________|\
      \________|\
      \___k____|\
      \________|\
      \___K____|\
      \________|\
      \________|\
      \________"
      -- White
      "________|\
      \________|\
      \________|\
      \__***___|\
      \__*_*___|\
      \__***___|\
      \________|\
      \________"
      -- Black
      "________|\
      \__***___|\
      \__*_*___|\
      \__***___|\
      \________|\
      \________|\
      \________|\
      \________"
  describe "dragon" $
    mkTest
      "r_bqkbnr|pp_ppp_p|__n___p_|________|___NP___|________|PPP__PPP|RNBQKB_R"
      -- White
      "________|________|*_*_*__*|_*_*_***|__**_**_|********|********|_******_"
      -- Black
      "_******_|********|********|*___**_*|_*_*____|________|________|________"

pawnPliesSpec :: Spec
pawnPliesSpec = describe "pawnPlies" $ do
  let TestBoard bd =
        read
          "___nk___|\
          \__P_P___|\
          \________|\
          \____p_pP|\
          \___p____|\
          \__p_____|\
          \_PPPP___|\
          \____K___"
      record =
        Record
          { placement = bd
          , activeColor = White
          , castling = none
          , enPassantTarget = Just g6
          , halfMove = 0
          , fullMove = 1
          }
      plyTable = legalPlies record
      expectSuccess tag ply withRecord =
        specify tag $
          case plyTable M.!? ply of
            Nothing ->
              fail $ "Expected " <> show ply <> " to be a legal move."
            Just r -> withRecord r
      expectFailure tag ply =
        specify tag $ plyTable M.!? ply `shouldSatisfy` isNothing
      matchBoard rawTestBoard r =
        TestBoard (placement r) `shouldBe` read rawTestBoard
  expectSuccess
    "b-pawn: simple advance"
    (PlyNorm b2 b3)
    $ matchBoard
      "___nk___|\
      \__P_P___|\
      \________|\
      \____p_pP|\
      \___p____|\
      \_Pp_____|\
      \__PPP___|\
      \____K___"
  expectSuccess
    "b-pawn: double advance & en passant tag"
    (PlyNorm b2 b4)
    $ \r -> do
      matchBoard
        "___nk___|\
        \__P_P___|\
        \________|\
        \____p_pP|\
        \_P_p____|\
        \__p_____|\
        \__PPP___|\
        \____K___"
        r
      enPassantTarget r `shouldBe` Just b3
  expectFailure
    "c-pawn: no move (simple)"
    (PlyNorm c2 c3)
  expectFailure
    "c-pawn: no move (advance)"
    (PlyNorm c2 c4)
  expectSuccess
    "d-pawn: simple advance"
    (PlyNorm d2 d3)
    $ matchBoard
      "___nk___|\
      \__P_P___|\
      \________|\
      \____p_pP|\
      \___p____|\
      \__pP____|\
      \_PP_P___|\
      \____K___"
  expectFailure
    "d-pawn: no double advance"
    (PlyNorm d2 d4)
  expectSuccess
    "d-pawn: can capture"
    (PlyNorm d2 c3)
    $ matchBoard
      "___nk___|\
      \__P_P___|\
      \________|\
      \____p_pP|\
      \___p____|\
      \__P_____|\
      \_PP_P___|\
      \____K___"
  expectSuccess
    "h-pawn: simple advance"
    (PlyNorm h5 h6)
    $ matchBoard
      "___nk___|\
      \__P_P___|\
      \_______P|\
      \____p_p_|\
      \___p____|\
      \__p_____|\
      \_PPPP___|\
      \____K___"
  expectFailure
    "h-pawn: no double advance"
    (PlyNorm h5 h7)
  expectSuccess
    "advance to promote"
    (PlyPromo c7 c8 Queen)
    $ matchBoard
      "__Qnk___|\
      \____P___|\
      \________|\
      \____p_pP|\
      \___p____|\
      \__p_____|\
      \_PPPP___|\
      \____K___"
  expectSuccess
    "capture to promote"
    (PlyPromo e7 d8 Knight)
    $ matchBoard
      "___Nk___|\
      \__P_____|\
      \________|\
      \____p_pP|\
      \___p____|\
      \__p_____|\
      \_PPPP___|\
      \____K___"
  expectSuccess
    "en passant"
    (PlyNorm h5 g6)
    $ matchBoard
      "___nk___|\
      \__P_P___|\
      \______P_|\
      \____p___|\
      \___p____|\
      \__p_____|\
      \_PPPP___|\
      \____K___"
  -- TODO: coverage for black pieces
