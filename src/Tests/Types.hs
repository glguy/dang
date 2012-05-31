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

-- | Generation of sigma types, parameterized on what sort of thing to generate
-- underneath.
sigmaType :: Gen Type -> Gen SigmaType
sigmaType  = fmap mkSigmaType . arbitraryForall
  where
  mkSigmaType = TScheme . fmap toQual

tauType :: Gen Type
tauType  = frequency
  [ (1, TApp   <$> tauType                   <*> tauType)
  , (2, TInfix <$> namespace qualName symbol <*> tauType <*> tauType)
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
