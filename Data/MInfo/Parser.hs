{-# LANGUAGE OverloadedStrings #-}

module Data.MInfo.Parser
  ( parseFile
  ) where

import           Prelude hiding     ( take )
import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Time

import Data.MInfo.Parser.Bson       ( parseDocument )
import Data.MInfo.Types


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
  line' = do
    d <- date year
    t <- parseThread <* spc
    c <- parseContent t
    return $ LogLine d t c


parseContent :: LogThread -> Parser LogContent
parseContent t =
  case t of
    (LtConnection _) ->
      (LcQuery <$> query "query") <|>
      (LcGetMore <$> query "getmore") <|>
      (LcUpdate <$> update) <|>
      (LcEndConnection <$> endConn) <|>
      other
    LtInitAndListen ->
      (LcAcceptConnection <$> acceptConn) <|>
      other
    _ ->
      other
 where
  other = toeol *> pure LcOther


typens :: BS.ByteString -> Parser BS.ByteString
typens t =
  string t *> char ' ' *> takeTill isSpace <* spc


acceptConn :: Parser BS.ByteString
acceptConn =
  string "connection accepted from " *> takeTill (== ':') <* toeol


endConn :: Parser BS.ByteString
endConn =
  string "end connection " *> takeTill (== ':') <* toeol


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


parseThread :: Parser LogThread
parseThread =
  char '[' *> thread <* char ']'


thread :: Parser LogThread
thread =
  connection <|>
  replicaSet <|>
  initAndListen <|>
  ttlMonitor <|>
  webServer <|>
  fileAllocator <|>
  other
 where
  initAndListen = "initandlisten" *> pure LtInitAndListen
  fileAllocator = "FileAllocator" *> pure LtFileAllocator
  ttlMonitor    = "TTLMonitor" *> pure LtTTLMonitor
  webServer     = "websvr" *> pure LtWebServer
  connection    = "conn" *> (LtConnection <$> decimal)
  other = LtOther <$> takeTill (== ']')


replicaSet :: Parser LogThread
replicaSet =
  string "rs" *> choice
    [ "HealthPoll" *> pure LtRsHealthPoll
    , "Mgr" *> pure LtRsManager
    , "GhostSync" *> pure LtRsGhostSync
    , "BackgroundSync" *> pure LtRsBackgroundSync
    , "SyncNotifier" *> pure LtRsSyncNotifier
    , "Sync" *> pure LtRsSync
    , "Start" *> pure LtRsStart
    ]


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
  -- first try to parse according to ISO8601 (>= mongodb-2.6)
  iso8601 <* char ' ' <|>
  -- otherwise try to parse the alternative format (<= mongodb-2.4)
  take 4 *> (UTCTime <$> day year <*> time) <* char ' '


day :: Integer -> Parser Day
day year =
  fromGregorian year <$> month <*> decimal <* char ' '


time :: Parser DiffTime
time = do
  (h, m, s, ms) <- hms
  return . picos $ time' h m s ms
 where
  time' h m s ms = (s + m * 60 + h * 3600) * 1000 + ms
  picos = picosecondsToDiffTime . (* 1000000000)


hms :: Parser (Integer, Integer, Integer, Integer)
hms = do
  h  <- decimal
  _  <- char ':'
  m  <- decimal
  _  <- char ':'
  s  <- decimal
  ms <- millis
  return (h, m, s, ms)
 where
  -- not every mongodb logs with milliseconds precision
  millis = char '.' *> decimal <|> return 0


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


correction :: Parser Integer
correction =
  (char '+' *> corr) <|>
  (char '-' *> (neg <$> corr)) <|>
  optional (char 'Z') *> return 0
 where
  -- try to parse either '02:00' or '0200'
  corr = tomin <$> decimal <*> (char ':' *> decimal <|> return 0)
  tomin h m
    | h < 100   = h * 60 + m
    | otherwise = (h `div` 100) * 60 + m
  neg = (* (-1))


iso8601 :: Parser UTCTime
iso8601 = do
  y <- decimal
  _ <- char '-'
  m <- decimal
  _ <- char '-'
  d <- decimal
  _ <- char 'T'
  (h, mi, s, ms) <- hms
  c <- correction
  return $
    UTCTime (fromGregorian y (fromInteger m) d)
            (time' h (mi + c) s ms)
 where
  time' h m s ms = picosecondsToDiffTime $
    ((s + m * 60 + h * 3600) * 1000 + ms) * 1000000000


spc :: Parser ()
spc = skipWhile (== ' ')


-- vim: set et sw=2 sts=2 tw=80:
