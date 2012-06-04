module Tests.Subsumption where

import Dang.IO (logInfo)
import Dang.Monad (runDangWithArgs)
import Tests.Types (scheme,monoType)
import TypeChecker.Monad (runTC)
import TypeChecker.Subsumption (subsumes)

import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck.Monadic (monadicIO,pick,run)


subsumptionTests :: Test
subsumptionTests  = testGroup "subsumption"
  [ testProperty "subsumption-refl" prop_subsumptionRefl
  ]

prop_subsumptionRefl = monadicIO $ do
  s <- pick scheme
  run (print s)
  run (runDangWithArgs [] (runTC (subsumes s s)))
