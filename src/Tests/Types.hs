{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Types where

import QualName (qualName)
import Tests.QualName (namespace,symbol,ident)
import Tests.Utils (reduce)
import TypeChecker.Types
import TypeChecker.Unify

import Control.Applicative (pure,(<$>),(<*>))
import Test.QuickCheck
import qualified Data.Set as Set


-- | This instance only ever generates unbound variables.
instance Arbitrary TVar where
  arbitrary = UVar <$> arbitrary

instance Arbitrary TParam where
  arbitrary = TParam
          <$> arbitrary
          <*> pure True
          <*> ident
          <*> arbitraryKind

-- | Generation of type schemes.
scheme :: Gen Scheme
scheme  = arbitraryForall (toQual <$> polyFun)

polyFun :: Gen PolyFun
polyFun  = PolyFun <$> listOf1 scheme  <*> monoType

monoType :: Gen Type
monoType  = frequency
  [ (1, TApp   <$> monoType                  <*> monoType)
  , (2, TInfix <$> namespace qualName symbol <*> monoType <*> monoType)
  , (4, TCon   <$> arbitrary)
  , (4, TVar   <$> arbitrary)
  ]

arbitraryKind :: Gen Kind
arbitraryKind  = oneof
  [ pure kstar
  , karrow <$> arbitraryKind <*> arbitraryKind
  ]

-- | Generation of quantified things.
arbitraryForall :: Types a => Gen a -> Gen (Forall a)
arbitraryForall gen = do
  ty <- gen
  let vars = Set.toList (typeVars ty)
  keep <- reduce vars
  return (quantify keep ty)
