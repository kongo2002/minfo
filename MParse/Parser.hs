{-# LANGUAGE OverloadedStrings #-}

module MParse.Parser where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe         ( catMaybes )
import           Data.Time
import           System.Locale      ( defaultTimeLocale )

import MParse.Parser.Bson           ( parseDocument )
import MParse.Types


loglines :: Integer -> Parser [LogLine]
loglines year =
  catMaybes <$> (logline year `sepBy` satisfy isEOL) <* end
 where
  end = toEOL *> endOfInput


logline :: Integer -> Parser (Maybe LogLine)
logline year = Just
  <$> line'
  <|> toEOL *> pure Nothing
 where
  line' = LogLine
    <$> date year
    <*> parseNamespace <* skipSpace
    <*> parseContent


parseContent :: Parser LogContent
parseContent =
  (LcQuery <$> query "query") <|>
  (LcGetMore <$> query "getmore") <|>
  other
 where
  other = LcOther <$> toEOL


query :: BS.ByteString -> Parser QueryInfo
query cmd = do
  _ <- string cmd
  _ <- space
  ns <- BS.unpack <$> takeTill isSpace
  skipSpace
  _ <- string "query: "
  skipSpace
  q <- parseDocument
  ci <- commandInfos
  return $ QueryInfo ns q ci


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
  catMaybes <$> ((skipSpace *> commandInfo) `sepBy` space)


commandInfo :: Parser (Maybe CommandInfo)
commandInfo = Just <$> choice
  [ ncom
  , info "cursorid" CiCursorId
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


-- vim: set et sw=2 sts=2 tw=80:
