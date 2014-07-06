module Data.MInfo.Operation
  ( getOperation
  ) where

import qualified Data.ByteString.Lazy as LBS

import Data.MInfo.Types
import Data.MInfo.Operation.Connections ( connections )
import Data.MInfo.Operation.Queries     ( queries )
import Data.MInfo.Operation.Restarts    ( restarts )


type Operation = [LogLine] -> LBS.ByteString


getOperation :: Options -> Operation
getOperation o =
  case oOperation o of
    Queries     -> queries (oSort o)
    Connections -> connections
    Restarts    -> restarts


-- vim: set et sw=2 sts=2 tw=80:
