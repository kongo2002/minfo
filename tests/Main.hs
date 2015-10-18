{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Attoparsec.ByteString.Char8 ( parseOnly )
import qualified Data.ByteString.Char8 as BS

import           Test.HUnit                       ( (@=?) )
import           Test.Framework                   ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit   ( testCase )

import           Data.MInfo.Parser                ( parseFile )
import           Data.MInfo.Parser.Bson           ( parseDocument )
import           Data.MInfo.Types


main :: IO ()
main = defaultMain tests


extract parser input =
  let (Right result) = parseOnly parser input
  in  result


bson :: BS.ByteString -> MongoElement
bson str = extract parseDocument str


parsed str =
  let info = ParserInfo (Nothing, Nothing) (const True) 2015
  in  length $ parseFile info str


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
      ]
    ]
  ]
