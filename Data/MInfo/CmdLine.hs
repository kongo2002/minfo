{-# LANGUAGE ViewPatterns #-}

module Data.MInfo.CmdLine
  ( parseOpts
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.List        ( inits )
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
    then return $ opt { oInput = readInput file, oFile = Just file }
    else errExit "specified file does not exist"

  -- determine sort order
  sortOrder "sum" o = return o { oSort = BySum }
  sortOrder "min" o = return o { oSort = ByMin }
  sortOrder "max" o = return o { oSort = ByMax }
  sortOrder "avg" o = return o { oSort = ByAvg }
  sortOrder _     _ = errExit "unknown sort order specified"


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
    , "and defaults to 'sum' if none is specified."
    , ""
    , "The output is written by default to stdout but may be"
    , "written to a file via the -o option."
    ]


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt RequireOrder options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= pos ps
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
        Nothing -> return $ o { oInput = readInput f }
        _       -> return o
    file _ o = return o

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
