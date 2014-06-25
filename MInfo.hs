{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List          ( sortBy )
import qualified Data.Map.Strict as M
import           Data.Monoid        ( mempty )
import           System.Environment ( getArgs )

import MInfo.CmdLine
import MInfo.Encoder
import MInfo.Parser
import MInfo.Types
import MInfo.Utils


type SortPredicate = (Int, Int, Int, Integer, [Int])
                  -> (Int, Int, Int, Integer, [Int])
                  -> Ordering

type MapKey   = (BS.ByteString, MongoElement)
type QueryMap = M.Map MapKey (Int, Int, Int, Integer, [Int])


aggregate :: [LogLine] -> QueryMap
aggregate ls =
  M.fromListWith group $ concatMap query ls
 where
  -- running calculation of length, min, max, sum
  group (_, _, _, _, [r]) (c, mi, ma, s, rs) =
   (c+1, min mi r, max ma r, s+toInteger r, r:rs)
  group _ rs   = rs

  query (LogLine _ _ content) =
    case content of
      (LcQuery (QueryInfo ns q ci))     -> [((ns, q), acc ci)]
      (LcGetMore (QueryInfo ns q ci))   -> [((ns, q), acc ci)]
      (LcUpdate (UpdateInfo ns q _ ci)) -> [((ns, q), acc ci)]
      _ -> []

  acc ci = (1, ms, ms, toInteger ms, ms')
   where
    (ms, ms') = getMs ci


getMs :: CommandInfo -> (Int, [Int])
getMs ci
  | ms > 0    = (ms, [ms])
  | otherwise = (0, [])
 where
  ms = ciRuntime ci


output :: SortPredicate -> QueryMap -> LBS.ByteString
output order qm =
  case input of
    [] -> LBS.empty
    xs -> toLazyByteString $ header <> foldr go mempty xs
 where
  nl      = charUtf8 '\n'
  tab     = stringUtf8 "    "
  ord a b = order (snd a) (snd b)
  input   = sortBy ord $ M.toList qm
  header  = table "NS:" "COUNT:" "MIN:" "MAX:" "AVG:" "SUM:"

  -- process one query aggregation
  go ((ns, q), t@(c, mi, ma, s, _)) acc =
    table
      (BS.unpack ns)
      (show c)
      (show mi)
      (show ma)
      (double2f $ avg t)
      (show s) <>
    tab <> encode q <> nl <> acc


avg :: (Int, Int, Int, Integer, [Int]) -> Double
avg (c, _, _, s, _) = fromIntegral s / fromIntegral c


byMin :: SortPredicate
byMin (_, s1, _, _, _) (_, s2, _, _, _) = compare s2 s1

byMax :: SortPredicate
byMax (_, _, s1, _, _) (_, _, s2, _, _) = compare s2 s1

bySum :: SortPredicate
bySum (_, _, _, s1, _) (_, _, _, s2, _) = compare s2 s1

byAvg :: SortPredicate
byAvg t1 t2 = compare (avg t2) (avg t1)


main :: IO ()
main = do
  opts     <- parseOpts =<< getArgs
  thisYear <- getCurrentYear

  LBS.putStr =<< process thisYear (oSort opts) <$> oInput opts
 where
  process y s = sorted s . aggregate . parseFile y

  sorted BySum = output bySum
  sorted ByMin = output byMin
  sorted ByMax = output byMax
  sorted ByAvg = output byAvg


-- vim: set et sw=2 sts=2 tw=80:
