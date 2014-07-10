{-# LANGUAGE ViewPatterns #-}

module Data.MInfo.CmdLine
  ( parseOpts
  ) where

import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List        ( inits )
import           Data.Maybe       ( fromMaybe )
import           Data.Time
import           System.Console.GetOpt
import           System.Directory ( doesFileExist )
import           System.Exit      ( exitWith, ExitCode(..), exitSuccess )
import           System.IO

import Data.MInfo.Types


getStdIn :: IO LBS.ByteString
getStdIn = LBS.hGetContents stdin


readInput :: String -> IO LBS.ByteString
readInput file =
  LBS.hGetContents =<< openFile file ReadMode


defOptions :: Options
defOptions = Options
  { oVerbose   = False
  , oFile      = Nothing
  , oInput     = getStdIn
  , oOutput    = LBS.putStrLn
  , oSort      = BySum
  , oOperation = Queries
  , oFrom      = Nothing
  , oTo        = Nothing
  }


options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "i" ["input"]
    (ReqArg
      useFile
      "FILE")
    "input file"

  , Option "o" ["output"]
    (ReqArg
      (\arg opt -> return opt { oOutput = LBS.writeFile arg })
      "FILE")
    "output file"

  , Option "s" ["sort"]
    (ReqArg
      sortOrder
      "ORDER")
    "sort order"

  , Option "f" ["from"]
    (ReqArg
      parseFrom
      "DATEFORMAT")
    "from date (optional)"

  , Option "t" ["to"]
    (ReqArg
      parseTo
      "DATEFORMAT")
    "to date (optional)"

  , Option "v" ["verbose"]
    (NoArg
      (\opt -> return opt { oVerbose = True }))
    "enable verbosity"

  , Option "V" ["version"]
    (NoArg
      (\_ -> err "minfo-0.0.1" >> exitSuccess))
    "print version"

  , Option "h" ["help"]
    (NoArg
      (\_ -> err usage >> exitSuccess))
    "show this help"
  ]
 where
  -- check file existance
  useFile file opt = do
    exists <- doesFileExist file
    if exists
    then return opt { oInput = readInput file, oFile = Just file }
    else errExit "specified file does not exist"

  -- parse from date
  parseFrom fmt opt = do
    now <- getCurrentTime
    case parseDate now fmt of
      Just date -> return opt { oFrom = Just date }
      Nothing   -> return opt

  -- parse to date
  parseTo fmt opt = do
    now <- getCurrentTime
    let from = fromMaybe now (oFrom opt)
    case parseDate from fmt of
      Just date -> return opt { oTo = Just date }
      Nothing   -> return opt

  -- determine sort order
  sortOrder "sum" o = return o { oSort = BySum }
  sortOrder "min" o = return o { oSort = ByMin }
  sortOrder "max" o = return o { oSort = ByMax }
  sortOrder "avg" o = return o { oSort = ByAvg }
  sortOrder "ns"  o = return o { oSort = ByNamespace }
  sortOrder _     _ = errExit "unknown sort order specified"


parseDate :: UTCTime -> String -> Maybe UTCTime
parseDate rel d =
  fmt d rel
 where
  minute = 60
  hour   = 60 * minute
  day    = 24 * hour

  add = addUTCTime . fromInteger
  sub = add . (*(-1))

  unit :: String -> Maybe Integer
  unit "s"       = Just 1
  unit "second"  = Just 1
  unit "seconds" = Just 1
  unit "h"       = Just hour
  unit "hour"    = Just hour
  unit "hours"   = Just hour
  unit "m"       = Just minute
  unit "minute"  = Just minute
  unit "minutes" = Just minute
  unit "d"       = Just day
  unit "day"     = Just day
  unit "days"    = Just day
  unit _         = Nothing

  fmt :: String -> UTCTime -> Maybe UTCTime
  fmt "today" n     = Just n
  fmt "tomorrow" n  = Just $ add day n
  fmt "yesterday" n = Just $ sub day n
  fmt ('+':xs) n    = fmt' add xs n
  fmt ('-':xs) n    = fmt' sub xs n
  fmt _ _           = Nothing

  fmt' func xs n =
    case int xs of
      Just (x, us) -> flip func n . (*x) <$> unit us
      _ -> Nothing

  int input =
    case reads input of
      []        -> Nothing
      [(_, [])] -> Nothing
      (x:_)     -> Just x


usage :: String
usage =
  usageInfo header options
 where
  header = unlines
    [ "Usage: minfo [COMMAND] [OPTION]... [FILE]"
    , ""
    , "The COMMAND to execute has to be one of the following:"
    , ""
    , " queries      accumulate queries by their execution time"
    , " connections  calculate connection state changes"
    , " restarts     filter for all (re)starts of the database"
    , ""
    , "In case no FILE and no -i option is specified the"
    , "input is read from stdin."
    , ""
    , "The sort order may be one of 'sum', 'min', 'max' and 'avg'"
    , "and defaults to 'sum' if none is specified. You may sort"
    , "by namespace ('ns') as well."
    , ""
    , "The output is written by default to stdout but may be"
    , "written to a file via the -o option."
    ]


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt Permute options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= pos ps >>= check
    -- errors
    (_, _, es) -> ioError (userError (concat es ++ usage))
 where
  -- process positional arguments
  pos [] _        = errExit usage
  pos (x:xs) opts =
    case op' x of
      Just operation -> file xs (opts { oOperation = operation })
      Nothing        -> errExit usage
   where
    file [f] o =
      case oFile opts of
        Nothing -> return o { oInput = readInput f }
        _       -> return o
    file _ o = return o

  check opts =
    case cmp of
      Just False -> errExit "invalid date range specified"
      _ -> return opts
   where
    cmp = (<) <$> oFrom opts <*> oTo opts

  op' (op "queries" -> True)     = Just Queries
  op' (op "connections" -> True) = Just Connections
  op' (op "restarts" -> True)    = Just Restarts
  op' _                          = Nothing


op :: String -> String -> Bool
op key input =
  input `elem` candidates key
 where
  candidates = tail . inits


err :: String -> IO ()
err = hPutStrLn stderr


errExit :: String -> IO a
errExit str = err str >> exitWith (ExitFailure 1)


-- vim: set et sw=2 sts=2 tw=80:
