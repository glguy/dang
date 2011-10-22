{-# LANGUAGE TypeSynonymInstances #-}

module TypeChecker.Env where

import Core.AST (Term)
import QualName (QualName)
import Pretty (Pretty(pp),declBlock,ppr,(<+>),text)
import TypeChecker.Unify (Types(..))

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set

type Assumps ty = Map.Map QualName (Assump ty)

instance Pretty ty => Pretty (Assumps ty) where
  pp _ = declBlock . map step . Map.toList
    where
    step (n,a) = ppr n <+> text "+->" <+> ppr a

instance Types ty => Types (Assumps ty) where
  apply u  = Map.map (apply u)
  typeVars = Fold.foldl step Set.empty
    where
    step acc a = acc `Set.union` typeVars a

data Assump ty = Assump
  { aBody :: Maybe Term
  , aData :: ty
  } deriving (Show)

instance Pretty ty => Pretty (Assump ty) where
  pp _ = ppr . aData

instance Types ty => Types (Assump ty) where
  apply u a = a { aData = apply u (aData a) }
  typeVars  = typeVars . aData

emptyAssumps :: Assumps ty
emptyAssumps  = Map.empty

lookupAssump :: QualName -> Assumps ty -> Maybe (Assump ty)
lookupAssump  = Map.lookup

addAssump :: QualName -> Assump ty -> Assumps ty -> Assumps ty
addAssump  = Map.insert

assumps :: Assumps ty -> [ty]
assumps  = map aData . Map.elems
