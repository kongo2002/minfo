module MInfo.Types where

import           Prelude hiding ( GT, LT )
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
  deriving ( Eq, Ord )


instance (Show MongoOperator) where
  show  GT            = "$gt"
  show  GTE           = "$gte"
  show  In            = "$in"
  show  LT            = "$lt"
  show  LTE           = "$lte"
  show  NE            = "$ne"
  show  NotIn         = "$nin"
  show  Or            = "$or"
  show  And           = "$and"
  show  Not           = "$not"
  show  Nor           = "$not"
  show  Exists        = "$exists"
  show  Type          = "$type"
  show  Mod           = "$mod"
  show  Regex         = "$regex"
  show  Text          = "$text"
  show  Where         = "$where"
  show  GeoWithin     = "$geoWithin"
  show  GeoIntersects = "$geoIntersects"
  show  Near          = "$near"
  show  NearSphere    = "$nearSphere"
  show  All           = "$all"
  show  ElemMatch     = "$elemMatch"
  show  Size          = "$size"
  show  Meta          = "$meta"
  show  Slice         = "$slice"


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
  | LcUpdate !UpdateInfo
  | LcOther
  deriving ( Show, Eq, Ord )


data QueryInfo = QueryInfo
  { qiNamespace :: BS.ByteString
  , qiQuery     :: MongoElement
  , qiInfos     :: CommandInfo
  } deriving ( Show, Eq, Ord )


data UpdateInfo = UpdateInfo
  { uiNamespace :: BS.ByteString
  , uiQuery     :: MongoElement
  , uiUpdate    :: MongoElement
  , uiInfos     :: CommandInfo
  } deriving ( Show, Eq, Ord )


data CommandInfo = CI
  { ciNScanned   :: Int
  , ciNReturned  :: Int
  , ciNToSkip    :: Int
  , ciNToReturn  :: Int
  , ciNDeleted   :: Int
  , ciNInserted  :: Int
  , ciResLen     :: Int
  , ciR          :: Int
  , ciKeyUpdated :: Int
  , ciCursorId   :: Int
  , ciNumYields  :: Int
  , ciRuntime    :: Int
  } deriving ( Show, Eq, Ord )


emptyCI :: CommandInfo
emptyCI = CI
  { ciNScanned   = -1
  , ciNReturned  = -1
  , ciNToSkip    = -1
  , ciNToReturn  = -1
  , ciNDeleted   = -1
  , ciNInserted  = -1
  , ciResLen     = -1
  , ciR          = -1
  , ciKeyUpdated = -1
  , ciCursorId   = -1
  , ciNumYields  = -1
  , ciRuntime    = -1
  }


data LogLine = LogLine
  { lTime      :: UTCTime
  , lNamespace :: LogNamespace
  , lContent   :: !LogContent
  } deriving ( Show, Eq, Ord )


-- vim: set et sw=2 sts=2 tw=80:
