{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.List          ( find )
import qualified Data.Map.Strict as M
import           Data.Time
import           System.Environment ( getArgs )

import MParse.Parser
import MParse.Types


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


aggregate :: [LogLine] -> M.Map MongoElement (Int, Int)
aggregate ls =
  M.fromListWith group ls'
 where
  group (_, 0) acc = acc
  group (_, t) (c', t') = (c'+1, t+t')

  ls' = [(qiQuery qi, (1, getMs (qiInfos qi))) | LogLine _ _ (LcQuery qi) <- ls]


getMs :: [CommandInfo] -> Int
getMs cs =
  get $ find ms cs
 where
  ms (CiRuntime _) = True
  ms _             = False

  get (Just (CiRuntime x)) = x
  get _                    = 0


main :: IO ()
main = do
  -- TODO: proper argument parsing
  [file]   <- getArgs
  thisYear <- getCurrentYear

  print =<< (aggregate . parseFile thisYear) <$> BL.readFile file


-- vim: set et sw=2 sts=2 tw=80:
