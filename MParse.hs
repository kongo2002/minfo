{-# LANGUAGE OverloadedStrings #-}

module MParse where

import           Control.Applicative

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe         ( catMaybes )
import           Data.Time

import           System.Locale      ( defaultTimeLocale )
import           System.Environment ( getArgs )

import MParse.BsonParser


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
  , qiQuery     :: MongoElement
  , qiInfos     :: [CommandInfo]
  } deriving ( Show, Eq, Ord )


data CommandInfo =
    CiNScanned Integer
  | CiNReturned Integer
  | CiNToSkip Integer
  | CiNToReturn Integer
  | CiNDeleted Integer
  | CiNInserted Integer
  | CiResLen Integer
  | CiR Integer
  | CiKeyUpdated Integer
  | CiRuntime Integer
  deriving ( Show, Eq, Ord )


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
  _ <- string "query: "
  skipSpace
  q <- parseDocument
  ci <- commandInfos
  return $ LcQuery $ QueryInfo ns q ci


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


commandInfos :: Parser [CommandInfo]
commandInfos =
  catMaybes <$> ((skipSpace *> commandInfo) `sepBy` char ' ')


commandInfo :: Parser (Maybe CommandInfo)
commandInfo = Just <$> choice
  [ ncom
  , info "keyUpdates" CiKeyUpdated
  , info "reslen" CiResLen
  , info "r" CiR
  , runtime
  ] <|> unknown
 where
  info s ctor = ctor <$> (string s *> char ':' *> num)
  unknown = skipWhile (not . isSpace) *> return Nothing
  ncom = char 'n' *> choice
    [ info "returned" CiNReturned
    , info "scanned" CiNScanned
    , info "inserted" CiNInserted
    , info "deleted" CiNDeleted
    , info "toreturn" CiNToReturn
    , info "toskip" CiNToSkip
    ]
  runtime = CiRuntime <$> num <* string "ms"


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
parseTime' year input =
  fmap corrected parsed
 where
  timeFormat = "%a %b %e %T%Q"
  parsed = parseTime defaultTimeLocale timeFormat input

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
