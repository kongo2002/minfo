{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import qualified Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 ( parseOnly )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import           Test.HUnit                       ( (@=?) )
import           Test.Framework                   ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit   ( testCase )

import           Data.MInfo.Parser                ( parseFile )
import           Data.MInfo.Parser.Bson           ( parseDocument )
import           Data.MInfo.Types


main :: IO ()
main = defaultMain tests


extract :: A.Parser a -> BS.ByteString -> a
extract parser input =
  let (Right result) = parseOnly parser input
  in  result


bson :: BS.ByteString -> MongoElement
bson str = extract parseDocument str


parsed :: BL.ByteString -> Int
parsed str =
  let info = ParserInfo (Nothing, Nothing) (const True) 2015
  in  length $ parseFile info str


one :: BL.ByteString -> LogContent
one str =
  let info = ParserInfo (Nothing, Nothing) (const True) 2015
  in  lContent $ head $ parseFile info str


query :: BL.ByteString -> MongoElement
query str =
  case one str of
    LcQuery q -> qiQuery q
    other     -> error $ "expected query, but got: " ++ show other


pipeline :: BL.ByteString -> [MongoElement]
pipeline str =
  case one str of
    LcAggregate a ->
      case aiPipeline a of
        MList ls -> ls
        other    -> error $ "expected pipeline, but got: " ++ show other
    other         -> error $ "expected aggregate, but got: " ++ show other


tests :: [Test]
tests =
  [ testGroup "BSON parsing"
    [ testGroup "values"
      [ testCase "empty document" (MObject [] @=? bson "{}")
      , testCase "empty document" (MObject [] @=? bson " {} ")
      , testCase "empty document" (MObject [] @=? bson "{  }")
      , testCase "number" (MValue @=? bson "12")
      , testCase "number" (MValue @=? bson "0.2352")
      , testCase "number" (MValue @=? bson "-152.23")
      , testCase "number" (MValue @=? bson " 52 ")
      , testCase "string" (MValue @=? bson "\"foo\"")
      , testCase "string" (MValue @=? bson "\"  foo  \"")
      , testCase "string" (MValue @=? bson "  \"  foo  \" ")
      , testCase "string" (MValue @=? bson "\"  { }  \"")
      , testCase "string" (MValue @=? bson "\" \\\"   \"")
      ]
    , testGroup "simplification"
      [ testCase "remove $explain" (MObject [] @=? bson "{$explain : 1}")
      , testCase "remove $explain" (MObject [(MKey "foo", MValue)] @=? bson "{$explain : 1, foo:4}")
      , testCase "flatten $query" (MObject [(MKey "foo", MValue)] @=? bson "{$query : {foo:32}}")
      , testCase "flatten $query" (MObject [(MKey "foo", MValue), (MKey "zap", MValue)] @=? bson "{$query : {foo:32,zap:0}}")
      ]
    ]
  , testGroup "date time parsing"
    [ testGroup "3.0 format"
      [ testCase "full" (1 @=? parsed "2015-10-18T20:22:20.825+0000 I CONTROL  [initandlisten] allocator: tcmalloc\n")
      , testCase "w/o time zone" (1 @=? parsed "2015-10-18T20:22:20.825 I CONTROL  [initandlisten] allocator: tcmalloc\n")
      , testCase "w/o time zone and milliseconds" (1 @=? parsed "2015-10-18T20:22:20 I CONTROL  [initandlisten] allocator: tcmalloc\n")
      ]
    ]
  , testGroup "basic parsing"
    [ testGroup "2.x format"
      [ testCase "initandlisten" (1 @=? parsed "2015-10-18T20:22:20.825+0000 [initandlisten] allocator: tcmalloc\n")
      ]
    , testGroup "3.x format"
      [ testCase "initandlisten" (1 @=? parsed "2015-10-18T20:22:20.825+0000 I CONTROL  [initandlisten] allocator: tcmalloc\n")
      , testCase "query #1" (1 @=? parsed "2015-11-28T19:40:37.830+0000 I QUERY    [conn8812] query stats.snapshots query: { v: { $in: [ 25252.0, 2562352.0 ] } } planSummary: COLLSCAN ntoreturn:0 ntoskip:0 nscanned:0 nscannedObjects:273788 keyUpdates:0 writeConflicts:0 numYields:2138 nreturned:0 reslen:20 locks:{ Global: { acquireCount: { r: 4278 } }, Database: { acquireCount: { r: 2139 } }, Collection: { acquireCount: { r: 2139 } } } 110ms\n")
      , testCase "query #2" (MObject [(MKey "v", MObject [(MOperator In, MList [MValue])])] @=? query "2015-11-28T19:40:37.830+0000 I QUERY    [conn8812] query stats.snapshots query: { v: { $in: [ 25252.0, 2562352.0 ] } } planSummary: COLLSCAN ntoreturn:0 ntoskip:0 nscanned:0 nscannedObjects:273788 keyUpdates:0 writeConflicts:0 numYields:2138 nreturned:0 reslen:20 locks:{ Global: { acquireCount: { r: 4278 } }, Database: { acquireCount: { r: 2139 } }, Collection: { acquireCount: { r: 2139 } } } 110ms\n")
      , testCase "aggregate #1" (1 @=? parsed "2015-11-28T20:00:12.053+0000 I COMMAND  [conn8818] command stats.$cmd command: aggregate { aggregate: \"snapshots\", pipeline: [ { $match: { k: \"MemoryUsage\", m: /^mono/ } }, { $group: { _id: \"$t\", res: { $push: { m: \"$m\", v: \"$v\" } }, sum: { $sum: \"$v\" } } }, { $sort: { _id: 1.0 } } ], cursor: {} } cursorid:13364525650 keyUpdates:0 writeConflicts:0 numYields:312 reslen:10093 locks:{ Global: { acquireCount: { r: 630 } }, Database: { acquireCount: { r: 315 } }, Collection: { acquireCount: { r: 315 } } } 110ms\n")
      , testCase "aggregate #2" (3 @=? (length $ pipeline "2015-11-28T20:00:12.053+0000 I COMMAND  [conn8818] command stats.$cmd command: aggregate { aggregate: \"snapshots\", pipeline: [ { $match: { k: \"MemoryUsage\", m: /^mono/ } }, { $group: { _id: \"$t\", res: { $push: { m: \"$m\", v: \"$v\" } }, sum: { $sum: \"$v\" } } }, { $sort: { _id: 1.0 } } ], cursor: {} } cursorid:13364525650 keyUpdates:0 writeConflicts:0 numYields:312 reslen:10093 locks:{ Global: { acquireCount: { r: 630 } }, Database: { acquireCount: { r: 315 } }, Collection: { acquireCount: { r: 315 } } } 110ms\n"))
      ]
    ]
  ]
