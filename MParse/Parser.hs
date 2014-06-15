{-# LANGUAGE OverloadedStrings #-}

module MParse.Parser where

import           Prelude hiding     ( take )
import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe         ( catMaybes )
import           Data.Time

import MParse.Parser.Bson           ( parseDocument )
import MParse.Types


parseFile :: Integer -> BL.ByteString -> [LogLine]
parseFile year ls =
  case AL.parse (logline year <* eol) ls of
    AL.Fail {}           -> []
    AL.Done ls' (Just l) -> l : parseFile year ls'
    AL.Done ls' _        -> parseFile year ls'


logline :: Integer -> Parser (Maybe LogLine)
logline year = Just <$> line'
  <|> toeol *> pure Nothing
 where
  line' = LogLine
    <$> date year
    <*> parseNamespace <* spc
    <*> parseContent


parseContent :: Parser LogContent
parseContent =
  (LcQuery <$> query "query") <|>
  (LcGetMore <$> query "getmore") <|>
  other
 where
  other = toeol *> pure LcOther


query :: BS.ByteString -> Parser QueryInfo
query cmd = do
  _ <- string cmd
  _ <- char ' '
  ns <- takeTill isSpace
  spc
  _ <- string "query: "
  spc
  q <- parseDocument
  spc
  ci <- commandInfos
  toeol
  return $ QueryInfo ns q ci


parseNamespace :: Parser LogNamespace
parseNamespace =
  char '[' *> namespace <* char ']'


namespace :: Parser LogNamespace
namespace =
  connection <|>
  healthPoll <|>
  ttlMonitor <|>
  initAndListen <|>
  webServer <|>
  fileAllocator <|>
  other
 where
  initAndListen = "initandlisten" *> pure NsInitAndListen
  healthPoll    = "rsHealthPoll" *> pure NsHealthPoll
  fileAllocator = "FileAllocator" *> pure NsFileAllocator
  ttlMonitor    = "TTLMonitor" *> pure NsTTLMonitor
  webServer     = "websvr" *> pure NsWebServer
  connection    = "conn" *> (NsConnection <$> decimal)
  other = NsOther <$> takeTill (== ']')


commandInfos :: Parser [CommandInfo]
commandInfos =
  catMaybes <$> (commandInfo `sepBy'` char ' ')


commandInfo :: Parser (Maybe CommandInfo)
commandInfo =
  mapCmd <$> takeWhile1 isAlpha_ascii <*> (char ':' *> spc *> decimal)
  <|> (Just . CiRuntime) <$> decimal <* string "ms"
  <|> unknown
 where
  unknown = skipWhile (/= ' ') *> pure Nothing

  mapCmd "cursorid" x   = Just $ CiCursorId x
  mapCmd "keyUpdates" x = Just $ CiKeyUpdated x
  mapCmd "nreturned" x  = Just $ CiNReturned x
  mapCmd "nscanned" x   = Just $ CiNScanned x
  mapCmd "ninserted" x  = Just $ CiNInserted x
  mapCmd "ndeleted" x   = Just $ CiNDeleted x
  mapCmd "numYields" x  = Just $ CiNumYields x
  mapCmd "ntoreturn" x  = Just $ CiNToReturn x
  mapCmd "ntoskip" x    = Just $ CiNToSkip x
  mapCmd "reslen" x     = Just $ CiResLen x
  mapCmd "r" x          = Just $ CiR x
  mapCmd _ _            = Nothing


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\r' || c == '\n'


toeol :: Parser ()
toeol = skipWhile (not . iseol)


date :: Integer -> Parser UTCTime
date year =
  take 4 *> (UTCTime <$> day year <*> time) <* char ' '


day :: Integer -> Parser Day
day year =
  fromGregorian year <$> month <*> decimal <* char ' '


time :: Parser DiffTime
time = do
  h <- decimal
  _ <- char ':'
  m <- decimal
  _ <- char ':'
  s <- decimal
  _ <- char '.'
  ms <- decimal
  return . picos $ time' h m s ms
 where
  time' h m s ms = (s + m * 60 + h * 3600) * 1000 + ms
  picos = picosecondsToDiffTime . (* 1000000000)


month :: Parser Int
month =
  month' <$> take 3 <* char ' '
 where
  month' "Jan" = 1
  month' "Feb" = 2
  month' "Mar" = 3
  month' "Apr" = 4
  month' "May" = 5
  month' "Jun" = 6
  month' "Jul" = 7
  month' "Aug" = 8
  month' "Sep" = 9
  month' "Oct" = 10
  month' "Nov" = 11
  month' "Dec" = 12
  month' _     = error "captain! we've been hit"


spc :: Parser ()
spc = skipWhile (== ' ')


-- vim: set et sw=2 sts=2 tw=80:
