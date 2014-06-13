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
    NsHealthPoll
  | NsWebServer
  | NsTTLMonitor
  | NsFileAllocator
  | NsInitAndListen
  | NsConnection Integer
  | NsOther String
  deriving ( Show, Eq, Ord )


data LogContent =
    LcQuery QueryInfo
  | LcOther BS.ByteString
  deriving ( Show, Eq, Ord )


data QueryInfo = QueryInfo
  { qiNamespace :: String
  , qiQuery     :: BS.ByteString
  } deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { lTime      :: UTCTime
  , lNamespace :: LogNamespace
  , lContent   :: LogContent
  } deriving ( Show, Eq, Ord )


parseLine :: Integer -> Parser LogLine
parseLine year = do
  d  <- date year
  ns <- parseNamespace <* skipSpace
  c  <- parseContent
  skipWhile isEOL
  return $ LogLine d ns c


parseContent :: Parser LogContent
parseContent =
  query <|> other
 where
  other = LcOther <$> toEOL


query :: Parser LogContent
query = do
  _ <- string "query "
  ns <- BS.unpack <$> takeTill isSpace
  skipSpace
  _ <- string "query:"
  skipSpace
  q <- toEOL
  return $ LcQuery $ QueryInfo ns q


parseNamespace :: Parser LogNamespace
parseNamespace =
  char '[' *> namespace <* char ']'


namespace :: Parser LogNamespace
namespace =
  initAndListen <|>
  healthPoll <|>
  connection <|>
  ttlMonitor <|>
  webServer <|>
  fileAllocator <|>
  other
 where
  initAndListen = string "initandlisten" *> pure NsInitAndListen
  healthPoll    = string "rsHealthPoll" *> pure NsHealthPoll
  fileAllocator = string "FileAllocator" *> pure NsFileAllocator
  ttlMonitor    = string "TTLMonitor" *> pure NsTTLMonitor
  webServer     = string "websvr" *> pure NsWebServer
  connection    = string "conn" *> (NsConnection <$> num)
  other = NsOther . BS.unpack <$> takeTill (== ']')


num :: Parser Integer
num = do
  n <- number
  case n of
    I n' -> return n'
    _    -> fail "invalid connection"


isEOL :: Char -> Bool
isEOL c = c == '\r' || c == '\n'


toEOL :: Parser BS.ByteString
toEOL = takeTill isEOL


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
