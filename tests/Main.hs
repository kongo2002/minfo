{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Data.Attoparsec.ByteString.Char8 ( parseOnly )
import qualified Data.ByteString.Char8 as BS

import           Test.HUnit                       ( (@=?), Assertion )
import           Test.Framework                   ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit   ( testCase )

import           Data.MInfo.Parser
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
    ]
  ]
