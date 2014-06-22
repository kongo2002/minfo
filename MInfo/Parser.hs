{-# LANGUAGE OverloadedStrings #-}

module MInfo.Parser where

import           Prelude hiding     ( take )
import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time

import MInfo.Parser.Bson           ( parseDocument )
import MInfo.Types


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
  (LcUpdate <$> update) <|>
  other
 where
  other = toeol *> pure LcOther


typens :: BS.ByteString -> Parser BS.ByteString
typens t =
  string t *> char ' ' *> takeTill isSpace <* spc


query' :: BS.ByteString -> Parser MongoElement
query' t =
  string t *> char ':' *> spc *> parseDocument <* spc


query :: BS.ByteString -> Parser QueryInfo
query t = do
  ns <- typens t
  q  <- query' "query"
  ci <- commandInfos
  toeol
  return $ QueryInfo ns q ci


update :: Parser UpdateInfo
update = do
  ns <- typens "update"
  q  <- query' "query"
  u  <- query' "update"
  ci <- commandInfos
  toeol
  return $ UpdateInfo ns q u ci


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


commandInfos :: Parser CommandInfo
commandInfos =
  commandInfo emptyCI >>= go
 where
  go ci = (char ' ' *> commandInfo ci >>= go) <|> pure ci


commandInfo :: CommandInfo -> Parser CommandInfo
commandInfo ci =
  mapCmd <$> takeWhile1 isAlpha_ascii <*> (char ':' *> spc *> decimal)
  <|> setRuntime <$> decimal <* string "ms"
  <|> unknown
 where
  unknown = skipWhile (/= ' ') *> pure ci

  setRuntime x = ci { ciRuntime = x }

  mapCmd "cursorid" x   = ci { ciCursorId = x }
  mapCmd "keyUpdates" x = ci { ciKeyUpdated = x }
  mapCmd "nreturned" x  = ci { ciNReturned = x }
  mapCmd "nscanned" x   = ci { ciNScanned = x }
  mapCmd "ninserted" x  = ci { ciNInserted = x }
  mapCmd "ndeleted" x   = ci { ciNDeleted = x }
  mapCmd "numYields" x  = ci { ciNumYields = x }
  mapCmd "ntoreturn" x  = ci { ciNToReturn = x }
  mapCmd "ntoskip" x    = ci { ciNToSkip = x }
  mapCmd "reslen" x     = ci { ciResLen = x }
  mapCmd "r" x          = ci { ciR = x }
  mapCmd _ _            = ci


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\n' || c == '\r'


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
