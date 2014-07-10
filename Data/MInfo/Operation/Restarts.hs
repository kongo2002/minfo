module Data.MInfo.Operation.Restarts where

import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.List   ( sortBy )
import           Data.Monoid ( mempty )
import           Data.Ord    ( comparing )

import Data.MInfo.Types
import Data.MInfo.Utils


restarts :: [LogLine] -> LBS.ByteString
restarts ls =
  toLazyByteString $ foldr go mempty sorted
 where
  starts = [(t, v) | (LogLine t _ (LcStart v)) <- ls]
  sorted = sortBy (comparing fst) starts

  go (t, v) acc =
    pad 30 (show t) <>
    byteString v <> charUtf8 '\n' <>
    acc


-- vim: set et sw=2 sts=2 tw=80:
