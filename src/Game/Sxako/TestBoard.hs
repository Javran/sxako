module Game.Sxako.TestBoard
  ( TestBoard (..)
  )
where

import Control.Monad
import Data.List
import Game.Sxako.Board
import Game.Sxako.Types
import Text.ParserCombinators.ReadP

{-
  Provides an alternative text representation of a Board
  for testing and debuging.
 -}
newtype TestBoard = TestBoard Board deriving (Eq)

instance Show TestBoard where
  show (TestBoard bd) =
    intercalate "|" . (fmap . fmap) sqToChar $ unpackToFenOrd bd
    where
      sqToChar = maybe '_' pieceToChar

instance Read TestBoard where
  readsPrec _ = readP_to_S $ do
    let sqP =
          (Nothing <$ char '_')
            <++ do
              ch <- get
              Just p <- pure (charToPiece ch)
              pure (Just p)
        lineP = replicateM 8 sqP
    x <- lineP
    xs <- replicateM 7 (char '|' *> lineP)
    let ys = x : xs
    pure $ TestBoard $ fromPlacement2d ys
