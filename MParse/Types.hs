module MParse.Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Time ( UTCTime )

data MongoOperator =
  -- comparison
    GT
  | GTE
  | In
  | LT
  | LTE
  | NE
  | NotIn
  -- logical
  | Or
  | And
  | Not
  | Nor
  -- element
  | Exists
  | Type
  -- evaluation
  | Mod
  | Regex
  | Text
  | Where
  -- geospatial
  | GeoWithin
  | GeoIntersects
  | Near
  | NearSphere
  -- array
  | All
  | ElemMatch
  | Size
  -- projection
  | Meta
  | Slice
  deriving ( Show, Eq, Ord )


data MongoKey =
    MKey BS.ByteString
  | MOperator MongoOperator
  deriving ( Show, Eq, Ord )


data MongoElement =
    MValue
  | MList [MongoElement]
  | MObject [(MongoKey, MongoElement)]
  deriving ( Show, Eq, Ord )


data LogNamespace =
    NsHealthPoll
  | NsWebServer
  | NsTTLMonitor
  | NsFileAllocator
  | NsInitAndListen
  | NsConnection Integer
  | NsOther BS.ByteString
  deriving ( Show, Eq, Ord )


data LogContent =
    LcQuery !QueryInfo
  | LcGetMore !QueryInfo
  | LcOther
  deriving ( Show, Eq, Ord )


data QueryInfo = QueryInfo
  { qiNamespace :: BS.ByteString
  , qiQuery     :: MongoElement
  , qiInfos     :: ![CommandInfo]
  } deriving ( Show, Eq, Ord )


data CommandInfo =
    CiNScanned !Int
  | CiNReturned !Int
  | CiNToSkip !Int
  | CiNToReturn !Int
  | CiNDeleted !Int
  | CiNInserted !Int
  | CiResLen !Int
  | CiR !Int
  | CiKeyUpdated !Int
  | CiRuntime !Int
  | CiCursorId !Int
  | CiNumYields !Int
  deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { lTime      :: BS.ByteString
  , lNamespace :: LogNamespace
  , lContent   :: !LogContent
  } deriving ( Show, Eq, Ord )


-- vim: set et sw=2 sts=2 tw=80:
