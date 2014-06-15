{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List          ( find )
import qualified Data.Map.Strict as M
import           Data.Time
import           System.Environment ( getArgs )

import MParse.Encoder
import MParse.Parser
import MParse.Types


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


output :: QueryMap -> BS.ByteString
output qm =
  -- TODO: do not intermingle IO and pretty printing
  M.foldrWithKey go BS.empty qm
 where
  go (ns, q) (c, mi, ma, s, ms) acc =
    let q'  = BS.concat $ BL.toChunks $ encodeLBS q
        str = ns `BS.append` ": " `BS.append` q' `BS.append` "\n"
    in acc `BS.append` str


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

  BS.putStrLn =<< process thisYear <$> BL.readFile file
 where
  process y = output . aggregate . parseFile y


-- vim: set et sw=2 sts=2 tw=80:
