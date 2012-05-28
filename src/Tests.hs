module Tests where

import Tests.RankN (rankNTests)

import Test.Framework

main :: IO ()
main  = defaultMain
  [ rankNTests
  ]
