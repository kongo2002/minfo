{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII ( intDec
                                                    , integerDec
                                                    , doubleDec )
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List          ( find )
import qualified Data.Map.Strict as M
import           Data.Monoid        ( mempty )
import           Data.Time
import           System.Environment ( getArgs )

import MParse.Encoder
import MParse.Parser
import MParse.Types
import MParse.Utils


type MapKey   = (BS.ByteString, MongoElement)
type QueryMap = M.Map MapKey (Int, Int, Int, Integer, [Int])


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


aggregate :: [LogLine] -> QueryMap
aggregate ls =
  M.fromListWith group ls'
 where
  -- running calculation of length, min, max, sum
  group (_, _, _, _, [x]) (c, mi, ma, s, acc) =
   (c+1, min mi x, max ma x, s+toInteger x, x:acc)
  group _ acc   = acc

  ls'     = [acc0 qi | LogLine _ _ (LcQuery qi) <- ls]

  acc0 qi = (key, (1, ms, ms, toInteger ms, ms'))
   where
    (ms, ms') = getMs (qiInfos qi)
    key       = (qiNamespace qi, qiQuery qi)


output :: QueryMap -> LBS.ByteString
output qm =
  toLazyByteString $ header <> M.foldrWithKey go mempty qm
 where
  nl     = charUtf8 '\n'
  tab    = charUtf8 '\t'
  double = stringUtf8 . double2f
  header = stringUtf8 "ns:\t\tcount:\tmin:\tmax:\tmean:\tsum:\n\n"

  -- process one query aggregation
  go (ns, q) (c, mi, ma, s, ms) acc =
    byteString ns <> tab <>
    intDec c <> tab <>
    intDec mi <> tab <>
    intDec ma <> tab <>
    double avg <> tab <>
    integerDec s <> nl <>
    tab <> encode q <> nl <> acc
   where
    avg = fromIntegral s / fromIntegral c


getMs :: [CommandInfo] -> (Int, [Int])
getMs cs =
  get $ find ms cs
 where
  ms (CiRuntime _) = True
  ms _             = False

  get (Just (CiRuntime x)) = (x, [x])
  get _                    = (0, [])


main :: IO ()
main = do
  -- TODO: proper argument parsing
  [file]   <- getArgs
  thisYear <- getCurrentYear

  LBS.putStr =<< process thisYear <$> LBS.readFile file
 where
  process y = output . aggregate . parseFile y


-- vim: set et sw=2 sts=2 tw=80:
