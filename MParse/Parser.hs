{-# LANGUAGE OverloadedStrings #-}

module MParse.Parser where

import           Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe         ( catMaybes )
import           Data.Time
import           System.Locale      ( defaultTimeLocale )

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
  ci <- commandInfos
  toeol
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
  initAndListen = "initandlisten" *> pure NsInitAndListen
  healthPoll    = "rsHealthPoll" *> pure NsHealthPoll
  fileAllocator = "FileAllocator" *> pure NsFileAllocator
  ttlMonitor    = "TTLMonitor" *> pure NsTTLMonitor
  webServer     = "websvr" *> pure NsWebServer
  connection    = "conn" *> (NsConnection <$> decimal)
  other = NsOther <$> takeTill (== ']')


commandInfos :: Parser [CommandInfo]
commandInfos =
  catMaybes <$> ((spc *> commandInfo) `sepBy'` char ' ')


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

{-
commandInfo :: Parser (Maybe CommandInfo)
commandInfo = Just <$> choice
  [ ncom
  , info "cursorid" CiCursorId
  , info "keyUpdates" CiKeyUpdated
  , r
  , runtime
  ] <|> unknown
 where
  info s ctor = ctor <$> (string s *> char ':' *> spc *> decimal)
  unknown = skipWhile (/= ' ') *> return Nothing
  ncom = char 'n' *> choice
    [ info "returned" CiNReturned
    , info "scanned" CiNScanned
    , info "inserted" CiNInserted
    , info "deleted" CiNDeleted
    , info "umYields" CiNumYields
    , to
    ]
  to = string "to" *> choice
    [ info "return" CiNToReturn
    , info "skip" CiNToSkip
    ]
  r = char 'r' *> choice
    [ info "eslen" CiResLen
    , CiR <$> (char ':' *> decimal)
    ]
  runtime = CiRuntime <$> decimal <* string "ms"
-}


eol :: Parser BS.ByteString
eol = takeWhile1 iseol


iseol :: Char -> Bool
iseol c = c == '\r' || c == '\n'


toeol :: Parser ()
toeol = skipWhile (not . iseol)


date :: Integer -> Parser BS.ByteString
date year =
  takeTill (== '[')
  {- case parseTime' year (BS.unpack d) of -}
    {- Just d' -> return d' -}
    {- Nothing -> fail "failed to parse date time" -}


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


spc :: Parser ()
spc = skipWhile (== ' ')


-- vim: set et sw=2 sts=2 tw=80:
