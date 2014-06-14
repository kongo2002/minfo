{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe         ( catMaybes )
import           Data.Time
import           System.Locale      ( defaultTimeLocale )
import           System.Environment ( getArgs )

import MParse.Parser


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


main :: IO ()
main = do
  -- TODO: proper argument parsing
  [file]   <- getArgs
  thisYear <- getCurrentYear

  let parse' = loglines thisYear

  print =<< parseOnly parse' <$> BS.readFile file

-- vim: set et sw=2 sts=2 tw=80:
