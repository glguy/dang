{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Renumber where

import QualName (Name)
import Syntax.AST (ConstrGroup(..),Constr(..))
import TypeChecker.Types
    (PolyFun(..),Type(..),Index,TParam(..),uvar,TVar(..),Forall(..),Qual(..))

import Control.Monad (ap)
import MonadLib (Id,StateT,get,set,runM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T


-- External Interface ----------------------------------------------------------

-- | Perform a syntactic renaming of the type variables.
renumber :: HasRenumber a => a -> a
renumber a = fst (runM (unRenumber (renumberVars a)) emptyRW)


-- Renumbering Monad -----------------------------------------------------------

newtype Renumber a = Renumber
  { unRenumber :: StateT RW Id a
  } deriving (Functor,Monad)

data RW = RW
  { rwIndex :: !Index
  , rwNames :: Map.Map Name Index
  } deriving (Show)

emptyRW :: RW
emptyRW  = RW
  { rwIndex = 0
  , rwNames = Map.empty
  }

addIndex :: Name -> RW -> (Index,RW)
addIndex n rw = (rwIndex rw,rw')
  where
  rw' = RW
    { rwIndex = rwIndex rw + 1
    , rwNames = Map.insert n (rwIndex rw) (rwNames rw)
    }

lookupIndex :: Name -> Renumber Index
lookupIndex n = Renumber $ do
  rw <- get
  case Map.lookup n (rwNames rw) of
    Just ix -> return ix
    Nothing -> do
      let (ix,rw') = addIndex n rw
      set rw'
      return ix


-- Term Renumbering ------------------------------------------------------------

class HasRenumber a where
  renumberVars :: a -> Renumber a

instance (HasRenumber a,HasRenumber b) => HasRenumber (a,b) where
  renumberVars (a,b) = (,) `fmap` renumberVars a `ap` renumberVars b

instance HasRenumber a => HasRenumber [a] where
  renumberVars = T.mapM renumberVars

instance HasRenumber a => HasRenumber (Maybe a) where
  renumberVars = T.mapM renumberVars

instance (Ord a, HasRenumber a) => HasRenumber (Set.Set a) where
  renumberVars sa = Set.fromList `fmap` mapM renumberVars (Set.toList sa)

instance HasRenumber PolyFun where
  renumberVars (PolyFun ps ty) =
    PolyFun `fmap` T.mapM renumberVars ps `ap` renumberVars ty

instance HasRenumber Type where
  renumberVars ty = case ty of
    TApp l r      -> TApp      `fmap` renumberVars l `ap` renumberVars r
    TInfix op l r -> TInfix op `fmap` renumberVars l `ap` renumberVars r
    TCon _        -> return ty
    TVar (GVar _) -> return ty
    TVar (UVar p) -> uvar `fmap` renumberVars p

instance HasRenumber TParam where
  renumberVars p = do
    ix <- lookupIndex (paramName p)
    return p { paramIndex = ix }

-- | Make the assumption that the parameters in the forall are already bound
-- here; renumbering should only happen on types that haven't already had a
-- forall put on them.
instance HasRenumber a => HasRenumber (Forall a) where
  renumberVars f = upd `fmap` renumberVars (forallData f)
    where
    upd a = f { forallData = a }

instance HasRenumber a => HasRenumber (Qual a) where
  renumberVars (Qual cxt a) = Qual `fmap` renumberVars cxt `ap` renumberVars a

instance HasRenumber ConstrGroup where
  renumberVars cg = do
    tys' <- renumberVars (groupArgs cg)
    cs'  <- T.mapM renumberVars (groupConstrs cg)
    return cg
      { groupArgs    = tys'
      , groupConstrs = cs'
      }

instance HasRenumber Constr where
  renumberVars c = do
    fs' <- T.mapM renumberVars (constrFields c)
    return c { constrFields = fs' }
