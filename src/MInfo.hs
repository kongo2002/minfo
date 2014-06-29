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
  year     <- getCurrentYear
  loglines <- oInput opts

  LBS.putStrLn $ getOperation opts (parseFile year loglines)


-- vim: set et sw=2 sts=2 tw=80:
