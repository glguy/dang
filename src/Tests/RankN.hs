{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.RankN where

import Tests.Types
import TypeChecker.Types
import TypeChecker.Subsumption

import Data.Maybe (fromMaybe)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test,testGroup)
import Test.QuickCheck


rankNTests :: Test
rankNTests  = testGroup "rank-n"
  [
  ]
