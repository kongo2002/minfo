{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Time
{- import           Data.Time.Format ( parseTime ) -}

import           System.Locale      ( defaultTimeLocale, iso8601DateFormat )
import           System.Environment ( getArgs )


data LogNamespace =
    LNHealthPoll
  | LNInitAndListen
  | LNConnection Integer
  | LNOther String
  deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { lTime      :: UTCTime
  , lNamespace :: LogNamespace
  , lContent   :: BS.ByteString
  } deriving ( Show, Eq, Ord )


parseLine :: Integer -> Parser LogLine
parseLine year = do
  d  <- date year
  ns <- parseNamespace <* skipSpace
  c  <- takeTill isEOL
  skipWhile isEOL
  return $ LogLine d ns c


parseNamespace :: Parser LogNamespace
parseNamespace =
  char '[' *> namespace <* char ']'


namespace :: Parser LogNamespace
namespace =
  initAndListen <|>
  healthPoll <|>
  connection <|>
  other
 where
  initAndListen = string "initandlisten" *> pure LNInitAndListen
  healthPoll    = string "rsHealthPoll" *> pure LNHealthPoll
  connection    = string "conn" *> (LNConnection <$> num)
  other = LNOther . BS.unpack <$> takeTill (== ']')


num :: Parser Integer
num = do
  n <- number
  case n of
    I n' -> return n'
    _    -> fail "invalid connection"


isEOL :: Char -> Bool
isEOL c = c == '\r' || c == '\n'


date :: Integer -> Parser UTCTime
date year = do
  d <- takeTill (== '[')
  case parseTime' year (BS.unpack d) of
    Just d' -> return d'
    Nothing -> fail "failed to parse date time"


parseTime' :: Integer -> String -> Maybe UTCTime
parseTime' year str =
  fmap corrected parsed
 where
  timeFormat = "%a %b %e %T%Q"
  parsed = parseTime defaultTimeLocale timeFormat str

  corrected u =
    UTCTime day' $ utctDayTime u
   where
    (_, month, day) = toGregorian $ utctDay u
    day'            = fromGregorian year month day


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


main :: IO ()
main = do
  [time] <- getArgs
  thisYear <- getCurrentYear

  print $ parseTime' thisYear time

-- vim: set et sw=2 sts=2 tw=80:
