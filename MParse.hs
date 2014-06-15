{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List          ( find )
import qualified Data.Map.Strict as M
import           Data.Time
import           System.Environment ( getArgs )

import MParse.Parser
import MParse.Types


type MapKey   = (BS.ByteString, MongoElement)
type QueryMap = M.Map MapKey (Int, [Int])


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


aggregate :: [LogLine] -> QueryMap
aggregate ls =
  M.fromListWith group ls'
 where
  -- calculate length on-the-fly
  group (_, [x]) (c, acc) = (c+1, x:acc)
  group _ acc   = acc

  key qi  = (qiNamespace qi, qiQuery qi)
  acc0 qi = (key qi, (1, getMs (qiInfos qi)))
  ls'     = [acc0 qi | LogLine _ _ (LcQuery qi) <- ls]


getMs :: [CommandInfo] -> [Int]
getMs cs =
  get $ find ms cs
 where
  ms (CiRuntime _) = True
  ms _             = False

  get (Just (CiRuntime x)) = [x]
  get _                    = []


main :: IO ()
main = do
  -- TODO: proper argument parsing
  [file]   <- getArgs
  thisYear <- getCurrentYear

  print =<< (aggregate . parseFile thisYear) <$> BL.readFile file


-- vim: set et sw=2 sts=2 tw=80:
