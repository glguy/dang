module Tests.Unification where

import Dang.Monad (runDangWithArgs)
import QualName (qualName)
import Tests.Monadic (assertFailure)
import Tests.QualName (namespace,conident)
import Tests.Types (monoType)
import TypeChecker.Types (uvar,Type(TCon))
import TypeChecker.Monad (runTC,unify,withSkolems)

import Test.Framework (Test,testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Monadic (monadicIO,pick,run)
import qualified Data.Set as Set


unificationTests :: Test
unificationTests  = testGroup "unification"
  [ testProperty "unify-skolem-refl" prop_unifySkolemRefl
  , testProperty "unify-skolem-fail" prop_unifySkolemFail
  , testProperty "unify-refl"        prop_unifyRefl
  ]

-- | A skolem should not unify with anything but a unification variable, or
-- itself.
prop_unifySkolemRefl = monadicIO $ do
  tparam <- pick arbitrary
  let var     = uvar tparam
      skolems = Set.singleton tparam
  run $ runDangWithArgs [] $ runTC $ withSkolems skolems $ unify var var

-- | A skolem should not unify with anything but a unification variable, or
-- itself.
prop_unifySkolemFail = monadicIO $ do
  tparam <- pick arbitrary
  name   <- pick (namespace qualName conident)
  let var     = uvar tparam
      con     = TCon name
      skolems = Set.singleton tparam
  run $ assertFailure [] $ runTC $ withSkolems skolems $ unify var con

prop_unifyRefl = monadicIO $ do
  ty <- pick monoType
  run $ runDangWithArgs [] $ runTC $ unify ty ty
