{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax.Types where

import Core.Types (Qual(..),toQual,putQual,getQual)
import Pretty
import QualName
import TypeChecker.Vars
import Variables

import Control.Applicative ((<$>),(<*>))
import Control.Monad (guard)
import Data.Data (Data)
import Data.Maybe (fromMaybe)
import Data.Serialize
    (Get,Putter,getWord8,putWord8,getListOf,putListOf,getSetOf,putSetOf)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Data.Set as Set


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall Kind (Qual PolyFun)

-- | Produce a type scheme that quantifies no variables.
toScheme :: Type -> Scheme
toScheme  = toForall . toQual . toPolyFun

putScheme :: Putter Scheme
putScheme  = putForall putKind (putQual putPolyFun)

getScheme :: Get Scheme
getScheme  = getForall getKind (getQual getPolyFun)


-- Polyfunctions ---------------------------------------------------------------

-- | A poly fun is a function whose arguments can be of higher rank.
data PolyFun = PolyFun [Scheme] Type
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift PolyFun where
  lift (PolyFun ps ty) = [| PolyFun $(lift ps) $(lift ty) |]

instance Pretty PolyFun where
  pp p (PolyFun ps ty) = optParens (p > 0) (sep funTy)
    where
    funTy = case ps of
      []   -> [ppr ty]
      f:fs ->  pp 1 f
            : [ppr arrowConstr <+> pp 1 a | a <- fs]
           ++ [ppr arrowConstr <+> ppr ty          ]

instance FreeVars PolyFun where
  freeVars (PolyFun sc ty) = freeVars sc `Set.union` freeVars ty

toPolyFun :: Type -> PolyFun
toPolyFun  = PolyFun []

putPolyFun :: Putter PolyFun
putPolyFun (PolyFun ps ty) = putListOf putScheme ps >> putType ty

getPolyFun :: Get PolyFun
getPolyFun  = PolyFun <$> getListOf getScheme <*> getType


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TInfix QualName Type Type
  | TCon QualName
  | TVar (TVar Kind)
    deriving (Eq,Show,Ord,Data,Typeable)

instance Lift Type where
  lift ty = case ty of
    TApp f x      -> [| TApp   $(lift f)  $(lift x)           |]
    TInfix qn l r -> [| TInfix $(lift qn) $(lift l) $(lift r) |]
    TCon qn       -> [| TCon   $(lift qn)                     |]
    TVar tv       -> [| TVar   $(lift tv)                     |]

instance Pretty Type where
  pp p ty = case ty of
    TApp a b     -> optParens (p > 1) (ppr a <+> pp 2 b)
    TInfix c a b -> optParens (p > 0) (pp 1 a <+> ppr c <+> pp 0 b)
    TCon n       -> ppr n
    TVar m       -> ppr m

instance FreeVars Type where
  freeVars ty = case ty of
    TApp a b      -> freeVars a `Set.union` freeVars b
    TInfix qn a b -> Set.singleton qn `Set.union` freeVars (a,b)
    TCon qn       -> Set.singleton qn
    TVar p        -> freeVars p

-- | Binary serialization for a @Type@.
putType :: Putter Type
putType ty = case ty of
  TApp l r     -> putWord8 0 >> putType l     >> putType r
  TInfix n l r -> putWord8 1 >> putQualName n >> putType l >> putType r
  TCon n       -> putWord8 2 >> putQualName n
  TVar p       -> putWord8 3 >> putTVar putKind p

-- | Binary parsing for a @Type@.
getType :: Get Type
getType  = getWord8 >>= \ tag ->
  case tag of
    0 -> TApp    <$> getType     <*> getType
    1 -> TInfix  <$> getQualName <*> getType <*> getType
    2 -> TCon    <$> getQualName
    3 -> TVar    <$> getTVar getKind
    _ -> fail ("Invalid Type tag: " ++ show tag)

-- | Construct a unification variable.
uvar :: TParam Kind -> Type
uvar  = TVar . UVar

-- | Construct a generic variable.
gvar :: TParam Kind -> Type
gvar  = TVar . GVar

-- | Type-application introduction.
tapp :: Type -> Type -> Type
tapp  = TApp

-- | Arrow introduction.
tarrow :: Type -> Type -> Type
tarrow  = TInfix arrowConstr
infixr 9 `tarrow`

arrowConstr :: QualName
arrowConstr  = primName ["Prelude"] "->"

intConstr :: QualName
intConstr  = primName ["Prelude"] "Int"

isTVar :: Type -> Bool
isTVar TVar{} = True
isTVar _      = False

destInfix :: Type -> Maybe (QualName,Type,Type)
destInfix (TInfix qn l r) = return (qn,l,r)
destInfix _               = Nothing

destArrow :: Type -> Maybe (Type,Type)
destArrow ty = do
  (qn,l,r) <- destInfix ty
  guard (qn == arrowConstr)
  return (l,r)

destArgs :: Type -> [Type]
destArgs ty = fromMaybe [ty] $ do
  (l,r) <- destArrow ty
  return (l:destArgs r)

destTApp :: Type -> Maybe (Type,Type)
destTApp (TApp l r) = Just (l,r)
destTApp _          = Nothing

destTCon :: Type -> [Type]
destTCon ty = fromMaybe [ty] $ do
  (l,r) <- destTApp ty
  return (l:destTCon r)

-- | Count the number of arguments to a function.
typeArity :: Type -> Int
typeArity ty = maybe 0 rec (destArrow ty)
  where
  rec (_,r) = 1 + typeArity r


-- Kinds -----------------------------------------------------------------------

type Kind = Type

putKind :: Putter Kind
putKind  = putType

getKind :: Get Kind
getKind  = getType

-- | The kind of types.
kstar :: Kind
kstar  = TCon (primName [] "*")

-- | The kind of constraints/contexts
kcxt :: Kind
kcxt  = TCon (primName [] "Cxt")

-- | The kind of type constructors.
karrow :: Kind -> Kind -> Kind
karrow  = TInfix (primName [] "->")
infixr 9 `karrow`


-- Sorts -----------------------------------------------------------------------

type Sort = Type

setSort :: Sort
setSort = TCon (primName ["Prelude"] "Set")
