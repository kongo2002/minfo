{-# LANGUAGE OverloadedStrings #-}
module MParse.MongoParser where

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


data MongoElement =
  ME MongoKey MongoValue
  deriving ( Show, Eq, Ord )


data MongoKey =
    MKey String
  | MOperator MongoOperator
  deriving ( Show, Eq, Ord )


data MongoValue =
    MValue
  | MList [MongoElement]
  deriving ( Show, Eq, Ord )


element :: Parser MongoElement
element = do
  skipSpace
  optional $ char '"'
  k <- key
  optional $ char '"'
  skipSpace
  char ':'
  skipSpace
  v <- value
  return $ ME k v


elements :: Parser [MongoElement]
elements =
  char '{' *> (element `sepBy` (char ',' <* skipSpace)) <* toEnd
 where
  toEnd = skipWhile (/= '}') *> char '}'


key :: Parser MongoKey
key =
  MOperator <$> operator <|>
  MKey . BS.unpack <$> takeTill keyEnd
 where
  -- TODO: fully implement keys in quotes
  keyEnd c = isSpace c || c == ':' || inClass "'\"" c


value :: Parser MongoValue
value =
  MList <$> elements <|>
  skipWhile valueEnd *> return MValue
 where
  valueEnd = notInClass ",{}"


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
