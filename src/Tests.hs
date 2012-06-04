module Tests where

import Tests.Subsumption (subsumptionTests)

import Test.Framework (defaultMain)

main :: IO ()
main  = defaultMain
  [ subsumptionTests
  ]
