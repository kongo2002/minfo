{-# LANGUAGE OverloadedStrings #-}

module Data.MInfo.Utils where

import Data.ByteString.Lazy.Builder
import Data.Monoid ( mappend, Monoid )
import Data.Time
import Numeric     ( showGFloat )


getCurrentYear :: IO Integer
getCurrentYear = do
  (year, _, _) <- fmap (toGregorian . utctDay) getCurrentTime
  return year


table :: String -> String -> String -> String -> String -> String -> Builder
table ns c mi ma avg s =
  pad 20 ns <>
  pad 10 c <>
  pad 10 mi <>
  pad 10 ma <>
  pad 10 avg <>
  trim 10 s <>
  charUtf8 '\n'


trim :: Int -> String -> Builder
trim l x = stringUtf8 $ take l x


pad :: Int -> String -> Builder
pad l x  = stringUtf8 $ take l (x ++ replicate l ' ') ++ " "


double2f :: Double -> String
double2f x = showGFloat (Just 2) x ""


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
