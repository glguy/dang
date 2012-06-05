module Tests where

import Tests.Subsumption (subsumptionTests)
import Tests.Unification (unificationTests)

import Test.Framework (defaultMain)

main :: IO ()
main  = defaultMain
  [ subsumptionTests
  , unificationTests
  ]
