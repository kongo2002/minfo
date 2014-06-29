module Data.MInfo.Operation.Connections where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

import Data.MInfo.Types
import Data.MInfo.Utils


type ConnectionMap = M.Map BS.ByteString (Int, Int)


connections :: [LogLine] -> LBS.ByteString
connections ls =
  LBS.empty


connections' :: [LogLine] -> ConnectionMap
connections' ls =
  M.fromListWith go $ concatMap conns ls
 where
  go (o, c) (o', c') = (o+o', c+c')

  conns (LogLine _ _ content) =
    case content of
      (LcAcceptConnection c) -> [(c, (1, 0))]
      (LcEndConnection c)    -> [(c, (0, 1))]
      _ -> []


-- vim: set et sw=2 sts=2 tw=80:
