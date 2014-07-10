module Data.MInfo.Types where

import           Prelude hiding ( GT, LT )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
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
  -- update
  | Inc
  | Mul
  | Rename
  | Unset
  | Set
  | SetOnInsert
  | Min
  | Max
  | CurrentDate
  | Pull
  | PullAll
  | Push
  | PushAll
  | AddToSet
  | Pop
  | Each
  | Sort
  | Position
  | Bit
  | Isolated
  -- commands (internal)
  | Explain
  | Query
  deriving ( Eq, Ord )


instance (Show MongoOperator) where
  show GT            = "$gt"
  show GTE           = "$gte"
  show In            = "$in"
  show LT            = "$lt"
  show LTE           = "$lte"
  show NE            = "$ne"
  show NotIn         = "$nin"
  show Or            = "$or"
  show And           = "$and"
  show Not           = "$not"
  show Nor           = "$not"
  show Exists        = "$exists"
  show Type          = "$type"
  show Mod           = "$mod"
  show Regex         = "$regex"
  show Text          = "$text"
  show Where         = "$where"
  show GeoWithin     = "$geoWithin"
  show GeoIntersects = "$geoIntersects"
  show Near          = "$near"
  show NearSphere    = "$nearSphere"
  show All           = "$all"
  show ElemMatch     = "$elemMatch"
  show Size          = "$size"
  show Meta          = "$meta"
  show Slice         = "$slice"
  show Inc           = "$inc"
  show Mul           = "$mul"
  show Rename        = "$rename"
  show Unset         = "$unset"
  show Set           = "$set"
  show SetOnInsert   = "$setOnInsert"
  show Min           = "$min"
  show Max           = "$max"
  show CurrentDate   = "$currentDate"
  show Pull          = "$pull"
  show PullAll       = "$pullAll"
  show Push          = "$push"
  show PushAll       = "$pushAll"
  show AddToSet      = "$addToSet"
  show Pop           = "$pop"
  show Each          = "$each"
  show Sort          = "$sort"
  show Position      = "$position"
  show Bit           = "$bit"
  show Isolated      = "$isolated"
  show Explain       = "$explain"
  show Query         = "$query"


data MongoKey =
    MKey BS.ByteString
  | MOperator MongoOperator
  deriving ( Show, Eq, Ord )


data MongoElement =
    MValue
  | MList [MongoElement]
  | MObject [(MongoKey, MongoElement)]
  deriving ( Show, Eq, Ord )


data LogThread =
    LtRsHealthPoll
  | LtRsManager
  | LtRsSync
  | LtRsSyncNotifier
  | LtRsStart
  | LtRsBackgroundSync
  | LtRsGhostSync
  | LtWebServer
  | LtTTLMonitor
  | LtFileAllocator
  | LtInitAndListen
  | LtConnection Integer
  | LtOther BS.ByteString
  deriving ( Show, Eq, Ord )


data LogContent =
    LcQuery !QueryInfo
  | LcGetMore !QueryInfo
  | LcUpdate !UpdateInfo
  | LcAcceptConnection BS.ByteString
  | LcEndConnection BS.ByteString
  | LcStart BS.ByteString
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
  { lTime    :: UTCTime
  , lThread  :: LogThread
  , lContent :: !LogContent
  } deriving ( Show, Eq, Ord )


data Operations =
    Queries
  | Connections
  | Restarts


data SortOrder =
    BySum
  | ByMin
  | ByMax
  | ByAvg
  | ByNamespace
  deriving ( Eq )


data Options = Options
  { oVerbose   :: Bool
  , oFile      :: Maybe String
  , oInput     :: IO LBS.ByteString
  , oOutput    :: LBS.ByteString -> IO ()
  , oSort      :: SortOrder
  , oOperation :: Operations
  , oFrom      :: Maybe UTCTime
  , oTo        :: Maybe UTCTime
  }


data ParserInfo = ParserInfo
  { piDateRange    :: Maybe (UTCTime, UTCTime)
  , piThreadFilter :: LogThread -> Bool
  , piYear         :: Integer
  }


-- vim: set et sw=2 sts=2 tw=80:
