module MInfo.CmdLine
  ( parseOpts
  , Options(..)
  ) where

import System.Console.GetOpt
import System.IO   ( hPutStrLn, stderr )
import System.Exit ( exitWith, ExitCode(..), exitSuccess )


data Options = Options
  { oVerbose :: Bool
  , oInput   :: String
  , oOutput  :: String
  } deriving ( Show, Eq )


defOptions :: Options
defOptions = Options
  { oVerbose = False
  , oInput   = ""
  , oOutput  = ""
  }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { oInput = arg })
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


usage :: String
usage = usageInfo "minfo" options


parseOpts :: [String] -> IO Options
parseOpts args =
  case getOpt RequireOrder options args of
    -- successful parsing
    (o, nos, []) -> do
      opts <- foldl (>>=) (return defOptions) o
      -- use positional argument in case
      -- the input file wasn't passed explicitely
      case (nos, oInput opts) of
        ([x], []) -> return $ opts { oInput = x }
        _ -> return $ opts
    -- errors
    (_, _, es) -> ioError (userError (concat es ++ usage))


err :: String -> IO ()
err = hPutStrLn stderr


-- vim: set et sw=2 sts=2 tw=80:
