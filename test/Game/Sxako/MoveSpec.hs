{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Game.Sxako.MoveSpec where

import Control.Monad
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import Data.Yaml as Yaml
import Game.Sxako.Castling
import Game.Sxako.Cli.TestDataGen
import Game.Sxako.Coord
import Game.Sxako.Fen
import Game.Sxako.Move
import Game.Sxako.TestBoard
import Game.Sxako.Types
import Paths_sxako
import Test.Hspec

spec :: Spec
spec = do
  attackingSquaresSpec
  pawnPliesSpec
  knightPliesSpec
  bishopPliesSpec
  rookPliesSpec
  queenPliesSpec
  kingPliesSpec
  examplesSpec
  testDataSpec

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

data TestUtils = TestUtils
  { expectSuccess :: String -> Ply -> (Record -> Expectation) -> Spec
  , expectFailure :: String -> Ply -> Spec
  , matchBoard :: String -> Record -> Expectation
  }

mkTestUtils :: Record -> TestUtils
mkTestUtils record = TestUtils {..}
  where
    plyTable = legalPliesMap record
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

pawnPliesSpec :: Spec
pawnPliesSpec = describe "pawnPlies" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "___nk___|\
            \__P_P___|\
            \________|\
            \____p_pP|\
            \__pp____|\
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
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "b-pawn: simple advance"
      (PlyNorm b2 b3)
      $ matchBoard
        "___nk___|\
        \__P_P___|\
        \________|\
        \____p_pP|\
        \__pp____|\
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
          \_Ppp____|\
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
        \__pp____|\
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
        \__pp____|\
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
        \__pp____|\
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
        \__pp____|\
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
        \__pp____|\
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
        \__pp____|\
        \__p_____|\
        \_PPPP___|\
        \____K___"
  describe "Black" $ do
    let TestBoard bd =
          read
            "____k___|\
            \_____p__|\
            \__p_____|\
            \___P__P_|\
            \pP______|\
            \________|\
            \p_______|\
            \____K___"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Just b3
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "f-pawn: simple advance"
      (PlyNorm f7 f6)
      $ matchBoard
        "____k___|\
        \________|\
        \__p__p__|\
        \___P__P_|\
        \pP______|\
        \________|\
        \p_______|\
        \____K___"
    expectSuccess
      "f-pawn: double advance & en passant tag"
      (PlyNorm f7 f5)
      $ \r -> do
        matchBoard
          "____k___|\
          \________|\
          \__p_____|\
          \___P_pP_|\
          \pP______|\
          \________|\
          \p_______|\
          \____K___"
          r
        enPassantTarget r `shouldBe` Just f6
    expectSuccess
      "c-pawn: capture"
      (PlyNorm c6 d5)
      $ matchBoard
        "____k___|\
        \_____p__|\
        \________|\
        \___p__P_|\
        \pP______|\
        \________|\
        \p_______|\
        \____K___"
    expectFailure
      "c-pawn: no double advance"
      (PlyNorm c6 c4)
    expectSuccess
      "a-pawn: promotion"
      (PlyPromo a2 a1 Rook)
      $ matchBoard
        "____k___|\
        \_____p__|\
        \__p_____|\
        \___P__P_|\
        \pP______|\
        \________|\
        \________|\
        \r___K___"
    expectSuccess
      "en passant"
      (PlyNorm a4 b3)
      $ matchBoard
        "____k___|\
        \_____p__|\
        \__p_____|\
        \___P__P_|\
        \________|\
        \_p______|\
        \p_______|\
        \____K___"

knightPliesSpec :: Spec
knightPliesSpec = describe "knightPlies" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "________|\
            \n_______|\
            \__p_____|\
            \_N______|\
            \___P____|\
            \P_P_____|\
            \________|\
            \________"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "b5c7"
      (PlyNorm b5 c7)
      $ matchBoard
        "________|\
        \n_N_____|\
        \__p_____|\
        \________|\
        \___P____|\
        \P_P_____|\
        \________|\
        \________"
    expectSuccess
      "b5d6"
      (PlyNorm b5 d6)
      $ matchBoard
        "________|\
        \n_______|\
        \__pN____|\
        \________|\
        \___P____|\
        \P_P_____|\
        \________|\
        \________"
    expectFailure
      "blocked by own piece"
      (PlyNorm b5 a3)
    expectSuccess
      "capture enemy piece"
      (PlyNorm b5 a7)
      $ matchBoard
        "________|\
        \N_______|\
        \__p_____|\
        \________|\
        \___P____|\
        \P_P_____|\
        \________|\
        \________"
  describe "Black" $ do
    let TestBoard bd =
          read
            "________|\
            \n_______|\
            \__p_____|\
            \_N______|\
            \___P____|\
            \P_P_____|\
            \________|\
            \________"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "a7c8"
      (PlyNorm a7 c8)
      $ matchBoard
        "__n_____|\
        \________|\
        \__p_____|\
        \_N______|\
        \___P____|\
        \P_P_____|\
        \________|\
        \________"
    expectFailure
      "blocked by own piece"
      (PlyNorm a7 c6)
    expectSuccess
      "capture enemy piece"
      (PlyNorm a7 b5)
      $ matchBoard
        "________|\
        \________|\
        \__p_____|\
        \_n______|\
        \___P____|\
        \P_P_____|\
        \________|\
        \________"

bishopPliesSpec :: Spec
bishopPliesSpec = describe "bishopPlies" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "___N___k|\
            \P_______|\
            \_____b__|\
            \________|\
            \___B____|\
            \________|\
            \________|\
            \_______K"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "d4c5"
      (PlyNorm d4 c5)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____b__|\
        \__B_____|\
        \________|\
        \________|\
        \________|\
        \_______K"
    expectSuccess
      "d4b6"
      (PlyNorm d4 b6)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_B___b__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \_______K"
    expectFailure
      "d4a7 (blocked by own piece)"
      (PlyNorm d4 a7)
    expectSuccess
      "d4c3"
      (PlyNorm d4 c3)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____b__|\
        \________|\
        \________|\
        \__B_____|\
        \________|\
        \_______K"
    expectSuccess
      "d4a1"
      (PlyNorm d4 a1)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____b__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \B______K"
    expectSuccess
      "d4g1"
      (PlyNorm d4 g1)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____b__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \______BK"
    expectSuccess
      "d4e5"
      (PlyNorm d4 e5)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____b__|\
        \____B___|\
        \________|\
        \________|\
        \________|\
        \_______K"
    expectSuccess
      "d4f6 (capture)"
      (PlyNorm d4 f6)
      $ matchBoard
        "___N___k|\
        \P_______|\
        \_____B__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \_______K"
    expectFailure
      "d4g7 (blocked by enemy piece)"
      (PlyNorm d4 g7)
  describe "Black" $ do
    let TestBoard bd =
          {-
            One needs to be careful with test input.
            h8 used to be a white rook which puts black king in check.
            We'll do some test cases to specifically cover
            checks and pins, but for now let's just verify
            behavior on bishops.
           -}
          read
            "___N___N|\
            \P______k|\
            \_____b__|\
            \________|\
            \___B____|\
            \________|\
            \________|\
            \______K_"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "f6h8 (capture)"
      (PlyNorm f6 h8)
      $ matchBoard
        "___N___b|\
        \P______k|\
        \________|\
        \________|\
        \___B____|\
        \________|\
        \________|\
        \______K_"
    expectSuccess
      "f6h4"
      (PlyNorm f6 h4)
      $ matchBoard
        "___N___N|\
        \P______k|\
        \________|\
        \________|\
        \___B___b|\
        \________|\
        \________|\
        \______K_"
    expectFailure
      "f6 a1 (blocked by enemy piece)"
      (PlyNorm f6 a1)

rookPliesSpec :: Spec
rookPliesSpec = describe "rookPlies" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "________|\
            \___R____|\
            \_k______|\
            \________|\
            \___r_p__|\
            \________|\
            \________|\
            \______K_"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm d7 d8)
      $ matchBoard
        "___R____|\
        \________|\
        \_k______|\
        \________|\
        \___r_p__|\
        \________|\
        \________|\
        \______K_"
    expectSuccess
      "capture"
      (PlyNorm d7 d4)
      $ matchBoard
        "________|\
        \________|\
        \_k______|\
        \________|\
        \___R_p__|\
        \________|\
        \________|\
        \______K_"
    expectFailure
      "blocked"
      (PlyNorm d7 d1)
  describe "Black" $ do
    let TestBoard bd =
          read
            "________|\
            \___R____|\
            \_k______|\
            \________|\
            \___r_p__|\
            \________|\
            \________|\
            \______K_"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm d4 a4)
      $ matchBoard
        "________|\
        \___R____|\
        \_k______|\
        \________|\
        \r____p__|\
        \________|\
        \________|\
        \______K_"
    expectSuccess
      "capture"
      (PlyNorm d4 d7)
      $ matchBoard
        "________|\
        \___r____|\
        \_k______|\
        \________|\
        \_____p__|\
        \________|\
        \________|\
        \______K_"
    expectFailure
      "blocked"
      (PlyNorm d4 h4)

queenPliesSpec :: Spec
queenPliesSpec = describe "queenPlies" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "K_______|\
            \_Q___n__|\
            \________|\
            \________|\
            \________|\
            \________|\
            \_B___q__|\
            \______k_"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm b7 d5)
      $ matchBoard
        "K_______|\
        \_____n__|\
        \________|\
        \___Q____|\
        \________|\
        \________|\
        \_B___q__|\
        \______k_"
    expectSuccess
      "capture"
      (PlyNorm b7 f7)
      $ matchBoard
        "K_______|\
        \_____Q__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \_B___q__|\
        \______k_"
    expectFailure
      "blocked"
      (PlyNorm b7 h7)
  describe "Black" $ do
    let TestBoard bd =
          read
            "K_______|\
            \_Q___n__|\
            \________|\
            \________|\
            \________|\
            \________|\
            \_B___q__|\
            \______k_"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm f2 h4)
      $ matchBoard
        "K_______|\
        \_Q___n__|\
        \________|\
        \________|\
        \_______q|\
        \________|\
        \_B______|\
        \______k_"

    expectSuccess
      "capture"
      (PlyNorm f2 b2)
      $ matchBoard
        "K_______|\
        \_Q___n__|\
        \________|\
        \________|\
        \________|\
        \________|\
        \_q______|\
        \______k_"
    expectFailure
      "blocked"
      (PlyNorm f2 a2)

kingPliesSpec :: Spec
kingPliesSpec = do
  kingNormalPliesSpec
  castlePliesSpec

kingNormalPliesSpec :: Spec
kingNormalPliesSpec = describe "kingPlies (normal moves)" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "________|\
            \________|\
            \__b_____|\
            \___pkp__|\
            \________|\
            \___PK___|\
            \____P_B_|\
            \________"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm e3 f3)
      $ matchBoard
        "________|\
        \________|\
        \__b_____|\
        \___pkp__|\
        \________|\
        \___P_K__|\
        \____P_B_|\
        \________"
    expectFailure
      "blocked"
      (PlyNorm e3 d3)
    expectFailure
      "not in check"
      (PlyNorm e3 e4)
  describe "Black" $ do
    let TestBoard bd =
          read
            "________|\
            \________|\
            \__b_____|\
            \___pkp__|\
            \________|\
            \___PK___|\
            \____P_B_|\
            \________"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = none
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "simple move"
      (PlyNorm e5 d6)
      $ matchBoard
        "________|\
        \________|\
        \__bk____|\
        \___p_p__|\
        \________|\
        \___PK___|\
        \____P_B_|\
        \________"
    expectFailure
      "blocked"
      (PlyNorm e5 f5)
    expectFailure
      "not in check"
      (PlyNorm e5 e4)

{-
  TODO: cover failures.
  TODO: probably time to seek coverage by checking against an engine:

  - assume stockfish:
    - "uci"
    - "setoption name MultiPV value 500"
    - "position ..."
    - "go depth 1"
    - wait for bestmove, collect multipv info.
    + then for each move:
    - "position ... moves ..."
    - "d" -> parse debugging info to extract FEN.

  Generate testdata from these results:

  format:

  "FEN {...}"
  "moves:"
  "  <move0>: <resulting FEN>"
  "  <move1>: <resulting FEN>"
  "  ..."
  "<empty line>"

  TODO: use yaml?
  TODO: two test type:
  - one to explore all moves from a given FEN
  - another to follow along a game.

 -}
castlePliesSpec :: Spec
castlePliesSpec = describe "kingPlies (castle)" $ do
  describe "White" $ do
    let TestBoard bd =
          read
            "r___k__r|\
            \ppp__ppp|\
            \________|\
            \________|\
            \________|\
            \________|\
            \PPP__PPP|\
            \R___K__R"
        record =
          Record
            { placement = bd
            , activeColor = White
            , castling = allAllowed
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "king side castle"
      (PlyNorm e1 g1)
      $ matchBoard
        "r___k__r|\
        \ppp__ppp|\
        \________|\
        \________|\
        \________|\
        \________|\
        \PPP__PPP|\
        \R____RK_"
    expectSuccess
      "queen side castle"
      (PlyNorm e1 c1)
      $ matchBoard
        "r___k__r|\
        \ppp__ppp|\
        \________|\
        \________|\
        \________|\
        \________|\
        \PPP__PPP|\
        \__KR___R"
  describe "Black" $ do
    let TestBoard bd =
          read
            "r___k__r|\
            \ppp__ppp|\
            \________|\
            \________|\
            \________|\
            \________|\
            \PPP__PPP|\
            \R___K__R"
        record =
          Record
            { placement = bd
            , activeColor = Black
            , castling = allAllowed
            , enPassantTarget = Nothing
            , halfMove = 0
            , fullMove = 1
            }
        TestUtils {..} = mkTestUtils record
    expectSuccess
      "king side castle"
      (PlyNorm e8 g8)
      $ matchBoard
        "r____rk_|\
        \ppp__ppp|\
        \________|\
        \________|\
        \________|\
        \________|\
        \PPP__PPP|\
        \R___K__R"
    expectSuccess
      "queen side castle"
      (PlyNorm e8 c8)
      $ matchBoard
        "__kr___r|\
        \ppp__ppp|\
        \________|\
        \________|\
        \________|\
        \________|\
        \PPP__PPP|\
        \R___K__R"

examplesSpec :: Spec
examplesSpec =
  describe "Examples" $
    specify "example #0" $ do
      let r = read @Record "4rrk1/R1Q3pp/8/1p6/8/1PP1p3/5RPP/6K1 b - - 0 26"
          rAfter = read @Record "4rrk1/R1Q3pp/8/1p6/8/1PP5/5pPP/6K1 w - - 0 27"
          mbd' = legalPliesMap r M.!? read "e3f2"
      mbd' `shouldBe` Just rAfter

{-
  For coverage of just those data samples:

  stack test --ta='--match "/Game.Sxako.Move/legalPlies.testdata"' --coverage
 -}
testDataSpec :: Spec
testDataSpec =
  describe "legalPlies.testdata" $ do
    tds <- runIO $ do
      fp <- getDataFileName "testdata/lichess-puzzles.yaml"
      r <- Yaml.decodeFileEither @[TestData] fp
      case r of
        Left msg ->
          error $ "Failed when loading testdata: " <> show msg
        Right v -> pure v
    forM_ tds $ \TestData {tdTag, tdPosition, tdLegalPlies} -> do
      specify (T.unpack tdTag) $ case tdLegalPlies of
        Nothing -> pending
        Just expectedLps ->
          legalPliesMap tdPosition `shouldBe` expectedLps
