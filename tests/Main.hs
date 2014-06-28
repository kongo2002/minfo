{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Attoparsec.ByteString.Char8 ( parseOnly )
import qualified Data.ByteString.Char8 as BS

import           Test.HUnit                       ( (@=?) )
import           Test.Framework                   ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit   ( testCase )

import           Data.MInfo.Parser.Bson           ( parseDocument )
import           Data.MInfo.Types


main :: IO ()
main = defaultMain tests


bson :: BS.ByteString -> MongoElement
bson str =
  let (Right result) = parseOnly parseDocument str
  in result


tests :: [Test]
tests =
  [ testGroup "BSON parsing"
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
  ]
