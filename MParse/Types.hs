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
    MKey String
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
  | NsOther String
  deriving ( Show, Eq, Ord )


data LogContent =
    LcQuery QueryInfo
  | LcGetMore QueryInfo
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
  | CiCursorId Integer
  deriving ( Show, Eq, Ord )


data LogLine = LogLine
  { lTime      :: UTCTime
  , lNamespace :: LogNamespace
  , lContent   :: LogContent
  } deriving ( Show, Eq, Ord )


-- vim: set et sw=2 sts=2 tw=80:
