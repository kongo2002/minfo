{-# LANGUAGE OverloadedStrings #-}
module MParse.BsonParser where

import           Prelude hiding ( GT, LT )
import           Control.Applicative

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS


data MongoOperator =
  -- comparison
    GT
  | GTE
  | In
  | LT
  | LTE
  | NE
  | NotIn
  -- logical
  | Or
  | And
  | Not
  | Nor
  -- element
  | Exists
  | Type
  -- evaluation
  | Mod
  | Regex
  | Text
  | Where
  -- geospatial
  | GeoWithin
  | GeoIntersects
  | Near
  | NearSphere
  -- array
  | All
  | ElemMatch
  | Size
  -- projection
  | Meta
  | Slice
  deriving ( Show, Eq, Ord )


data MongoKey =
    MKey String
  | MOperator MongoOperator
  deriving ( Show, Eq, Ord )


data MongoElement =
    MValue
  | MList [MongoElement]
  | MObject [(MongoKey, MongoElement)]
  deriving ( Show, Eq, Ord )


parseDocument :: Parser MongoElement
parseDocument =
  skipSpace *> element '}' <* skipSpace


object :: Parser (MongoKey, MongoElement)
object = do
  skipSpace
  optional $ char '"'
  k <- key
  optional $ char '"'
  skipSpace
  char ':'
  skipSpace
  v <- element '}'
  return $ (k, v)


objects :: Parser [(MongoKey, MongoElement)]
objects =
  between '{' '}' (object `sepBy` (char ',' <* skipSpace))


list :: Parser [MongoElement]
list =
  between '[' ']' (element ']' `sepBy` (char ',' <* skipSpace))


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
  MKey . BS.unpack <$> takeTill keyEnd
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
  char '$' *>
    (gt <|> lt <|> in' <|> nin <|> and <|> or)
 where
  gt  = string "gt" *> ((char 'e' *> pure GTE) <|> pure GT)
  lt  = string "lt" *> ((char 'e' *> pure LTE) <|> pure LT)
  in' = string "in" *> pure In
  nin = string "nin" *> pure NotIn
  and = string "and" *> pure And
  or  = string "or" *> pure Or


-- vim: set et sw=2 sts=2 tw=80:
