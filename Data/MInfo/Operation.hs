module Data.MInfo.Operation
  ( operation
  , parserFilter
  ) where

import qualified Data.ByteString.Lazy as LBS

import Data.MInfo.Types
import Data.MInfo.Operation.Connections ( connections )
import Data.MInfo.Operation.Queries     ( queries )
import Data.MInfo.Operation.Restarts    ( restarts )


type Operation = [LogLine] -> LBS.ByteString


operation :: Options -> Operation
operation o =
  case oOperation o of
    Queries     -> queries (oSort o)
    Connections -> connections
    Restarts    -> restarts


parserFilter :: Operations -> LogThread -> Bool
parserFilter op =
  case op of
    Queries     -> isConn
    Connections -> \x -> x == LtInitAndListen || isConn x
    Restarts    -> (== LtInitAndListen)
 where
  isConn (LtConnection _) = True
  isConn _                = False


-- vim: set et sw=2 sts=2 tw=80:
