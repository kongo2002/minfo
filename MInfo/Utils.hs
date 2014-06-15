{-# LANGUAGE OverloadedStrings #-}

module MInfo.Utils where

import Data.Monoid ( mappend, Monoid )
import Numeric     ( showGFloat )


double2f :: Double -> String
double2f x = showGFloat (Just 2) x ""


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}


-- vim: set et sw=2 sts=2 tw=80:
