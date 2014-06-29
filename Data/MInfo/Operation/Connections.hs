module Data.MInfo.Operation.Connections where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy.Builder
import           Data.List   ( sortBy )
import qualified Data.Map.Strict as M
import           Data.Monoid ( mempty )
import           Data.Ord    ( comparing )

import Data.MInfo.Types
import Data.MInfo.Utils


type ConnectionMap = M.Map BS.ByteString (Int, Int)


connections :: [LogLine] -> LBS.ByteString
connections ls =
  case ls of
    [] -> LBS.empty
    _  -> toLazyByteString $ header <> foldr go mempty (sort ls)
 where
  go (k, (o, c)) acc =
    pad 20 (BS.unpack k) <>
    pad 12 (show o) <>
    pad 12 (show c) <>
    nl <> acc

  header = pad 20 "IP:" <> pad 12 "CONN:" <> pad 12 "DISCONN:" <> nl
  sort   = sortBy (comparing byConn) . M.toList . connections'
  byConn = fst . snd
  nl     = charUtf8 '\n'


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
