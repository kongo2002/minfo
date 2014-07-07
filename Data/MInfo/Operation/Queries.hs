module Data.MInfo.Operation.Queries
  ( queries
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.List          ( sortBy )
import qualified Data.Map.Strict as M
import           Data.Monoid        ( mempty )

import Data.MInfo.Encoder
import Data.MInfo.Types
import Data.MInfo.Utils


data QV = QV
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Int
  !Integer
  ![Int]


type SortPredicate = (MapKey, QV) -> (MapKey, QV) -> Ordering

type MapKey   = (BS.ByteString, MongoElement)
type QueryMap = M.Map MapKey QV


queries :: SortOrder -> [LogLine] -> LBS.ByteString
queries s =
  output (sort s) . queries'
 where
  sort BySum       = bySum
  sort ByMin       = byMin
  sort ByMax       = byMax
  sort ByAvg       = byAvg
  sort ByNamespace = byNamespace


output :: SortPredicate -> QueryMap -> LBS.ByteString
output order qm =
  case input of
    [] -> LBS.empty
    xs -> toLazyByteString $ header <> foldr go mempty xs
 where
  nl      = charUtf8 '\n'
  tab     = stringUtf8 "    "
  input   = sortBy order $ M.toList qm
  header  = table "NS:" "COUNT:" "MIN:" "MAX:" "AVG:" "SUM:"

  -- process one query aggregation
  go ((ns, q), t@(QV c mi ma s _)) acc =
    table
      (BS.unpack ns)
      (show c)
      (show mi)
      (show ma)
      (double2f $ avg t)
      (show s) <>
    tab <> encode q <> nl <> acc


queries' :: [LogLine] -> QueryMap
queries' ls =
  M.fromListWith group $ concatMap query ls
 where
  -- running calculation of length, min, max, sum
  group (QV _ _ _ _ [r]) (QV c mi ma s rs) =
   QV (c+1) (min mi r) (max ma r) (s+toInteger r) (r:rs)
  group _ rs   = rs

  query (LogLine _ _ content) =
    case content of
      (LcQuery (QueryInfo ns q ci))     -> [((ns, q), acc ci)]
      (LcGetMore (QueryInfo ns q ci))   -> [((ns, q), acc ci)]
      (LcUpdate (UpdateInfo ns q _ ci)) -> [((ns, q), acc ci)]
      _ -> []

  acc ci = QV 1 ms ms (toInteger ms) ms'
   where
    (ms, ms') = getMs ci


getMs :: CommandInfo -> (Int, [Int])
getMs ci
  | ms > 0    = (ms, [ms])
  | otherwise = (0, [])
 where
  ms = ciRuntime ci


avg :: QV -> Double
avg (QV c _ _ s _) = fromIntegral s / fromIntegral c


byMin :: SortPredicate
byMin (_, QV _ s1 _ _ _) (_, QV _ s2 _ _ _) = compare s2 s1

byMax :: SortPredicate
byMax (_, QV _ _ s1 _ _) (_, QV _ _ s2 _ _) = compare s2 s1

bySum :: SortPredicate
bySum (_, QV _ _ _ s1 _) (_, QV _ _ _ s2 _) = compare s2 s1

byAvg :: SortPredicate
byAvg (_, t1) (_, t2) = compare (avg t2) (avg t1)

byNamespace :: SortPredicate
byNamespace (n1, _) (n2, _) = compare n1 n2


-- vim: set et sw=2 sts=2 tw=80:
