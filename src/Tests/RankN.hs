{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Tests.RankN where

import Tests.Types
import TypeChecker.Types
import TypeChecker.Subsumption

import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test,testGroup)
import Test.QuickCheck


rankNTests :: Test
rankNTests  = testGroup "rank-n"
  [ testProperty "Weak Prenix Conversion (mono)"
    prop_weakPrenixConversion_mono
  , testProperty "Weak Prenix Conversion (monofun)"
    prop_weakPrenixConversion_mono
  ]

-- | Test that for any mono-type, promoting it to a sigma-type, then performing
-- weak-prenix-conversion will have no effect.
prop_weakPrenixConversion_mono =
  forAll tauType $ \ ty ->
    let sigma     = sigmaTauType ty
        (ps,qrho) = weakPrenixConversion sigma
     in null ps && sigma == TScheme (toForall qrho)

-- | Test that weak-prenix-conversion of a functionof mono-types doesn't change
-- the underlying mono-types.
prop_weakPrenixConversion_monoFun =
  forAll tauType $ \ l ->
  forAll tauType $ \ r ->
    let sl        = sigmaTauType l
        sr        = sigmaTauType r
        sigma     = sigmaRhoType (rhoSigmaFun sl sr)
        (ps,qrho) = weakPrenixConversion sigma
     in null ps && sigma == TScheme (toForall qrho)
