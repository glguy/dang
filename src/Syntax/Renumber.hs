module Syntax.Renumber where

import QualName (Name)
import Syntax.AST (ConstrGroup(..),Constr(..))
import TypeChecker.Types
    (PolyFun(..),Type(..),Index,TParam(..),uvar,TVar(..),Forall(..),Qual(..))

import Control.Monad (ap)
import Control.Monad.ST (ST,runST)
import Data.STRef (STRef,newSTRef,readSTRef,writeSTRef)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as T


-- | Perform a syntactic renaming of the type variables.
renumber :: Renumber a => a -> a
renumber a = runST body
  where
  body = do
    ref <- initialRenumberState
    renumberVars ref a


type RenumberState s = STRef s (Index, Map.Map Name Index)

initialRenumberState :: ST s (RenumberState s)
initialRenumberState  = newSTRef (0, Map.empty)


class Renumber a where
  renumberVars :: RenumberState s -> a -> ST s a

instance (Renumber a,Renumber b) => Renumber (a,b) where
  renumberVars st (a,b) =
    (,) `fmap` renumberVars st a `ap` renumberVars st b

instance Renumber a => Renumber [a] where
  renumberVars st = T.mapM (renumberVars st)

instance Renumber a => Renumber (Maybe a) where
  renumberVars st = T.mapM (renumberVars st)

instance (Ord a, Renumber a) => Renumber (Set.Set a) where
  renumberVars st sa = do
    es <- mapM (renumberVars st) (Set.toList sa)
    return (Set.fromList es)

instance Renumber PolyFun where
  renumberVars st (PolyFun ps ty) =
    PolyFun `fmap` T.mapM (renumberVars st) ps `ap` renumberVars st ty

instance Renumber Type where
  renumberVars ref ty = case ty of
    TApp l r      -> TApp      `fmap` renumberVars ref l `ap` renumberVars ref r
    TInfix op l r -> TInfix op `fmap` renumberVars ref l `ap` renumberVars ref r
    TCon _        -> return ty
    TVar (GVar _) -> return ty
    TVar (UVar p) -> uvar `fmap` renumberVars ref p

instance Renumber TParam where
  renumberVars ref p = do
    (n,m) <- readSTRef ref
    let var = paramName p
    case Map.lookup var m of
      Just ix -> return p { paramIndex = ix }
      Nothing -> do
        writeSTRef ref (n+1,Map.insert var n m)
        return p { paramIndex = n }

-- | Make the assumption that the parameters in the forall are already bound
-- here; renumbering should only happen on types that haven't already had a
-- forall put on them.
instance Renumber a => Renumber (Forall a) where
  renumberVars st f = upd `fmap` renumberVars st (forallData f)
    where
    upd a = f { forallData = a }

instance Renumber a => Renumber (Qual a) where
  renumberVars st (Qual cxt a) =
    Qual `fmap` renumberVars st cxt `ap` renumberVars st a

instance Renumber ConstrGroup where
  renumberVars st cg = do
    tys' <- renumberVars st (groupArgs cg)
    cs'  <- T.mapM (renumberVars st) (groupConstrs cg)
    return cg
      { groupArgs    = tys'
      , groupConstrs = cs'
      }

instance Renumber Constr where
  renumberVars st c = do
    fs' <- T.mapM (renumberVars st) (constrFields c)
    return c { constrFields = fs' }
