module Tests.RankN where

import Tests.Types
import TypeChecker.Types
import TypeChecker.Unify

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test,testGroup)
import Test.QuickCheck


rankNTests :: Test
rankNTests  = testGroup "rank-n"
  [ testProperty "Weak Prenix Conversion (mono)" prop_weakPrenixConversion_mono
  ]

-- | Test that for any mono-type, promoting it to a sigma-type, then performing
-- weak-prenix-conversion will have no effect.
prop_weakPrenixConversion_mono ty = null ps && sigma == toForall qrho
  where
  sigma     = toSigmaType ty
  (ps,qrho) = weakPrenixConversion sigma

