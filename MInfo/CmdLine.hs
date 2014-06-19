module MInfo.CmdLine
  ( parseOpts
  , Options(..)
  ) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           System.Console.GetOpt
import           System.Directory ( doesFileExist )
import           System.Exit      ( exitWith, ExitCode(..), exitSuccess )
import           System.IO


data Options = Options
  { oVerbose :: Bool
  , oFile    :: Maybe String
  , oInput   :: IO LBS.ByteString
  , oOutput  :: String
  }


getStdIn :: IO LBS.ByteString
getStdIn = LBS.hGetContents stdin


readInput :: String -> IO LBS.ByteString
readInput file =
  LBS.hGetContents =<< openFile file ReadMode


defOptions :: Options
defOptions = Options
  { oVerbose = False
  , oFile    = Nothing
  , oInput   = getStdIn
  , oOutput  = ""
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
      (\arg opt -> return opt { oOutput = arg })
      "FILE")
    "output file"

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
    "show help"
  ]
 where
  useFile file opt = do
    exists <- doesFileExist file
    if exists
    then return $ opt { oInput = readInput file, oFile = Just file }
    else errExit "specified file does not exist"


usage :: String
usage = usageInfo "minfo" options


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt RequireOrder options args of
    -- successful
    (o, ps, []) ->
      foldl (>>=) (return defOptions) o >>= pos ps
    -- errors
    (_, _, es) -> ioError (userError (concat es ++ usage))
 where
  -- use positional argument in case
  -- the input file wasn't passed explicitely
  pos [x] opts =
    case oFile opts of
      Nothing -> return $ opts { oInput = readInput x }
      _       -> return opts
  pos _ opts = return opts


err :: String -> IO ()
err = hPutStrLn stderr


errExit :: String -> IO a
errExit str = err str >> exitWith (ExitFailure 1)


-- vim: set et sw=2 sts=2 tw=80:
