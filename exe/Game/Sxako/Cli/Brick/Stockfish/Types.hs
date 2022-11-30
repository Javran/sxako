module Game.Sxako.Cli.Brick.Stockfish.Types (
  ScoreBound (..),
  ScorePart (..),
  Score,
  pprScore,
  Ply,
  pprPly,
  PvInfo,
  pprDep,
  SfIn (..),
  SfStat (..),
  SfState (..),
  InfoComp (..),
  GameExtra,
) where

{-
  TODO: this to be cleaned up and merged into sxako library,
  as part of UCI support.
 -}

import Data.Monoid
import qualified Data.Text as T
import Data.Time
import GHC.Generics
import Game.Sxako.Common
import Game.Sxako.Fen

data ScoreBound = ScLowerbound | ScUpperbound deriving (Show)
data ScorePart = CentiPawn Int | Mate Int deriving (Show)
type Score = (ScorePart, Maybe ScoreBound)

pprScore :: Color -> Score -> String
pprScore c (sp, b) =
  prefix <> case sp of
    CentiPawn v -> show @Double (fromIntegral (wp v) / 100)
    Mate v -> "M" <> show (wp v)
  where
    wp = case c of
      White -> id
      Black -> negate
    prefix = case b of
      Nothing -> ""
      Just ScUpperbound -> "<="
      Just ScLowerbound -> ">="

type Ply = (String, String, Maybe Char)

pprPly :: Ply -> String
pprPly (a, b, c) = a <> b <> maybe "" (: []) c

type PvInfo = ({- depth / seldepth -} (Int, Maybe Int), Score, [Ply])

pprDep :: (Int, Maybe Int) -> String
pprDep (d, sd) = maybe id (\v -> (<> ".." <> show v)) sd $ show d

type GameExtra = ((T.Text, T.Text), UTCTime)

data SfIn
  = SfInFen Record (Maybe GameExtra)
  | SfInQuit

data SfStat = SfStat
  { ssNodes :: Last Int
  , ssNps :: Last Int
  , ssHashFull :: Last Int
  , ssTbHits :: Last Int
  , ssCpuLoad :: Last Int
  , ssTime :: Last Int
  }
  deriving (Generic, Show)

-- TODO: just awful.
instance Semigroup SfStat where
  SfStat a0 b0 c0 d0 e0 f0 <> SfStat a1 b1 c1 d1 e1 f1 =
    SfStat (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1) (e0 <> e1) (f0 <> f1)

instance Monoid SfStat where
  mempty = SfStat mempty mempty mempty mempty mempty mempty

data SfState = SfState
  { sfStat :: SfStat
  , sfPvs :: [PvInfo]
  , sfPosition :: Maybe Record
  , sfGameExtra :: Maybe GameExtra
  }
  deriving (Show)

data InfoComp
  = Depth Int
  | SelDepth Int
  | Time Int
  | Nodes Int
  | Pv [Ply]
  | MultiPv Int
  | Score Score
  | CurrMove Ply
  | CurrMoveNumber Int
  | HashFull Int
  | Nps Int
  | TbHits Int
  | CpuLoad Int
  | InfoStr String
  deriving (Show)

-- no support for `refutation` and `currline` - we don't have plan to enable those in the first place.
