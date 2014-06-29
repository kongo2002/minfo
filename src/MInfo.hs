module Main where

import qualified Data.ByteString.Lazy.Char8 as LBS
import           System.Environment ( getArgs )

import Data.MInfo.CmdLine
import Data.MInfo.Operation
import Data.MInfo.Parser
import Data.MInfo.Types
import Data.MInfo.Utils


main :: IO ()
main = do
  opts     <- parseOpts =<< getArgs
  thisYear <- getCurrentYear

  ls <- oInput opts
  LBS.putStrLn $ getOperation opts (parseFile thisYear ls)


-- vim: set et sw=2 sts=2 tw=80:
