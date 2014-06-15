{-# LANGUAGE OverloadedStrings #-}

module MParse.Parser.Bson where

import           Prelude hiding ( GT, LT )
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

import           MParse.Types


parseDocument :: Parser MongoElement
parseDocument =
  skipSpace *> element '}' <* skipSpace


object :: Parser (MongoKey, MongoElement)
object = do
  skipSpace
  _ <- optional $ char '"'
  k <- key
  _ <- optional $ char '"'
  skipSpace
  _ <- char ':'
  skipSpace
  v <- element '}'
  return (k, v)


objects :: Parser [(MongoKey, MongoElement)]
objects =
  between '{' '}' (object `sepBy` (char ',' <* skipSpace))


list :: Parser [MongoElement]
list =
  between '[' ']' (element ']' `sepBy` (char ',' <* skipSpace))


between :: Char -> Char -> Parser a -> Parser a
between start end parser =
  char start *> (parser <* toEnd)
 where
  toEnd = skipWhile (/= end) *> char end


str :: Parser String
str =
  char '"' *> many character <* char '"'
 where
  character  = (char '\\' *> escape) <|> satisfy (`BS.notElem` "\"\\")
  escape     = choice $ zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/"
  decode c r = r <$ char c


key :: Parser MongoKey
key =
  MOperator <$> operator <|>
  MKey <$> takeTill keyEnd
 where
  -- TODO: fully implement keys in quotes
  keyEnd c = isSpace c || c == ':' || inClass "'\"" c


element :: Char -> Parser MongoElement
element end =
  skipSpace *> element'
 where
  element' =
    MObject <$> objects <|>
    MList <$> list <|>
    value end


value :: Char -> Parser MongoElement
value end =
  (str <|> (BS.unpack <$> takeTill valueEnd)) *> return MValue
 where
  valueEnd c = c == end || c == ','


operator :: Parser MongoOperator
operator =
  char '$' *> choice
    [ gt
    , lt
    , op "ne" NE
    , op "in" In
    , op "nin" NotIn
    , op "all" All
    , op "elemMatch" ElemMatch
    , op "size" Size
    , op "exists" Exists
    , op "and" And
    , op "or" Or
    , op "not" Not
    , op "nor" Nor
    , op "where" Where
    , op "regex" Regex
    , op "mod" Mod
    , op "text" Text
    , op "geoWithin" GeoWithin
    , op "geoIntersects" GeoIntersects
    , op "near" Near
    , op "nearSphere" NearSphere
    , op "type" Type
    , op "meta" Meta
    , op "slice" Slice
    ]
 where
  op s c = string s *> pure c
  gt     = string "gt" *> ((char 'e' *> pure GTE) <|> pure GT)
  lt     = string "lt" *> ((char 'e' *> pure LTE) <|> pure LT)


-- vim: set et sw=2 sts=2 tw=80:
