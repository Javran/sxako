{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Game.Sxako.Pgn
  ( TagPair
  , tagPairP
  , stringLitP
  , sanSuffixP
  , MovetextElem (..)
  , mtElemP
  , MovetextResult (..)
  , movetextResultP
  , pgnP
  , manyPgnsP
  )
where

{-
  TODO: parsing PGN file.

  Reference: http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
  - 18: Formal syntax

  Import and export format: it appears that import format is just a lax form
  of the export format, meaning that a parser accepting import format is sufficient
  to handle export format without changes.

  We might need to break parsing down into two steps:

  - parsing the file literally.
  - interpret movetext following along FEN.

  Movetext has the problem that it can only be interpreted
  properly given current FEN. So instead of making parsing stateful
  (in the sense that we "validate" a movetext as we replay
  from starting FEN), we tries to interpret the result
  only after parsing is done.

  For a FEN record, we generate all plies in `Ply` datatype,
  and then come up with another type mapping all plies
  to short algebratic notation.

  TODO: implementation plan.

  - first round to recognize tag pairs and movetext section, without verification
    + parsing tag pairs
    + parsing movetext
  - second round to turn this intermediate structure into a more organized one,
    verification included.

 -}

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Functor
import qualified Data.Text as T
import Data.Text.Encoding
import Game.Sxako.Common
import Game.Sxako.San

type TagPair = (T.Text, T.Text)

tok :: Parser a -> Parser a
tok = (<* skipSpace)

{-
  PGN spec isn't clear on this one, let's just handle the most common ones, namely:

  - \n (most common)
  - \r \n
  - \r

 -}
newlineP :: Parser ()
newlineP = lf <|> (cr *> option () lf)
  where
    cr = void $ char '\r'
    lf = void $ char '\n'

stringLitP :: Parser T.Text
stringLitP =
  tr
    <$> (char '"'
           *> many
             (noEscapeChunk
                <|> charToBuilder <$> escapedChar)
             <* char '"')
  where
    tr :: [Builder.Builder] -> T.Text
    tr =
      decodeLatin1
        . BSL.toStrict
        . Builder.toLazyByteString
        . mconcat

    {-
      A quote inside a string is represented by the backslash immediately followed by a quote.
      A backslash inside a string is represented by two adjacent backslashes.
     -}
    noEscapeChunk =
      Builder.byteString
        <$> Parser.takeWhile1 (`notElem` ['\\', '\"'])
    charToBuilder :: Char -> Builder.Builder
    charToBuilder = Builder.int8 . fromIntegral . ord
    escapedChar =
      char '\\'
        *> (('"' <$ char '"') <|> ('\\' <$ char '\\'))

{-
  Parses a single tag pair.

  TODO: one line can contain multiple tag pairs. so we can
  probably try `many (many1 tagPairP <* newlineP)`, which should
  consume all tag pair lines, leading us to the empty line signaling
  start of movetext section.

 -}
tagPairP :: Parser TagPair
tagPairP =
  tok (char '[')
    *> ((,)
          <$> (decodeLatin1 <$> tok (Parser.takeWhile1 isTagSymbol))
            <*> tok stringLitP)
    <* char ']'
  where
    {-
      A further restriction on tag names is that they are composed exclusively of
      letters, digits, and the underscore character.
     -}
    isTagSymbol ch = isAlpha_iso8859_15 ch || Parser.isDigit ch || ch == '_'

{-
  a tag pair section is 0 to many non-empty lines of tag pairs
  (separated by non-newline whitespaces on each line),
  followed by an empty newline.
 -}
tagPairSectionP :: Parser [TagPair]
tagPairSectionP = (concat <$> many tagPairLine) <* newlineP
  where
    tagPairLine :: Parser [TagPair]
    tagPairLine =
      many1
        (tagPairP
           <* skipWhile nonNlSpace)
        <* newlineP
    nonNlSpace ch = Parser.isSpace ch && ch /= '\r' && ch /= '\n'

{-
  Short for MoveText element.

  Note that PGN spec says just that "Comment text may appear in PGN data"
  without being specific about where it could appear.
  So for now let's just keep it simple and just assume brace-surrounding
  commentaries are only available in movetext section.

  Note that MovetextElem is not structural, but rather tokens to enable further passing.

  TODO: is there any value support commentary in tag pair sections?

  TODO: plan for further passing:

  - probably megaparsec would come into play - at next round of passing
    MovetextElem *are* the tokens.

  - commentary are not meant to present alone:

    + consecutive comments are collected into a list
    + a comment right after a move number is attached to that move
    + a comment right after a ply is attached to that ply
    + a special case is when comments appear right before anything else for movetext section,
      in which case we should have a "pretext" field to contain it.

 -}
data MovetextElem
  = MtMoveNum Int
  | MtSan San (Maybe Int {- Int for NAG, suffix annotation will be translated into NAG. -})
  | MtCommentary T.Text
  | MtRav [MovetextElem]
  deriving (Show)


{-
  Parsing suffix of a SAN ply as NAG.
 -}
sanSuffixP :: Parser Int
sanSuffixP =
  (char '!' *> option 1 (3 <$ char '!' <|> 5 <$ char '?'))
    <|> (char '?' *> option 2 (6 <$ char '!' <|> 4 <$ char '?'))
    <?> "sanSuffixP"

{-

  Note that in order to make parsing easier,
  SAN move suffix annotations and NAG cannot both present for the same ply.

 -}
mtElemP :: Parser MovetextElem
mtElemP =
  mtMoveNumP
    <|> mtSanP
    <|> mtCommentaryP
    <|> mtRavP
    <?> "mtElemP"
  where
    mtMoveNumP =
      MtMoveNum
        <$> (decimal <* skipSpace <* Parser.takeWhile (== '.'))
    mtSanP = do
      s <- sanP
      let sufOrNag =
            {-
              A SAN suffix must immediate follow SAN,
              but NAG token is allowed to be separated with
              some spaces between as it is considered a single token.
             -}
            sanSuffixP
              <|> (skipSpace *> char '$' *> decimal)
      n <- option Nothing (Just <$> sufOrNag)
      pure $ MtSan s n
    mtCommentaryP =
      MtCommentary <$> do
        _ <- tok (char '{')
        xs <- Parser.takeWhile (/= '}')
        _ <- char '}'
        -- TODO: probably trim spaces and collapse newlines into spaces.
        pure (decodeLatin1 xs)
    mtRavP =
      MtRav <$> do
        _ <- tok (char '(')
        xs <- many (tok mtElemP)
        _ <- char ')'
        pure xs

data MovetextResult
  = MtrWon Color
  | MtrDrawn
  | MtrUnknown
  deriving (Eq, Show)

movetextResultP :: Parser MovetextResult
movetextResultP =
  MtrUnknown <$ char '*'
    <|> MtrWon Black <$ string "0-1"
    <|> (char '1'
           *> (MtrWon White <$ string "-0"
                 <|> MtrDrawn <$ "/2-1/2"))
    <?> "movetextResultP"

movetextSectionP :: Parser ([MovetextElem], MovetextResult)
movetextSectionP = endP <|> continueP <?> "movetextSectionP"
  where
    endP = ([],) <$> movetextResultP
    continueP = do
      e <- tok mtElemP
      first (e :) <$> movetextSectionP

type PgnRep = ([TagPair], ([MovetextElem], MovetextResult))

pgnP :: Parser PgnRep
pgnP = (,) <$> tagPairSectionP <*> movetextSectionP

manyPgnsP :: Parser [PgnRep]
manyPgnsP = many (tok pgnP)
