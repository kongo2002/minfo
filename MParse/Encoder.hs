{-# LANGUAGE OverloadedStrings #-}

module MParse.Encoder
  ( encode
  , encodeLBS
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder

import           MParse.Types
import           MParse.Utils


encodeLBS :: MongoElement -> LBS.ByteString
encodeLBS = toLazyByteString . encode


encode :: MongoElement -> Builder
encode MValue = charUtf8 '1'


encode (MList []) = stringUtf8 "[]"
encode (MList xs) =
  charUtf8 '[' <>
  encode (head xs) <>
  foldr go (charUtf8 ']') (tail xs)
 where
  go x a = stringUtf8 ", " <> encode x <> a


encode (MObject []) = stringUtf8 "{}"
encode (MObject xs) =
  stringUtf8 "{ " <>
  encodeKV (head xs) <>
  foldr go (stringUtf8 " }") (tail xs)
 where
  go x a = stringUtf8 ", " <> encodeKV x <> a


encodeKV :: (MongoKey, MongoElement) -> Builder
encodeKV (k, v) =
  encodeKey k <> stringUtf8 ": " <> encode v


encodeKey :: MongoKey -> Builder
encodeKey op =
  charUtf8 '"' <> enc op <> charUtf8 '"'
 where
  enc (MOperator o) = stringUtf8 $ show o
  enc (MKey k)      = escape k

  escape bs =
    case BS.uncons t of
      Nothing      -> byteString h
      Just (c, cs) -> byteString h <>
                      charUtf8 '\\' <>
                      charUtf8 c <>
                      escape cs
   where
    isEscape c = c == '"' ||
                 c == '\n' ||
                 c == '\t'
    (h, t) = BS.break isEscape bs


-- vim: set et sw=2 sts=2 tw=80:
