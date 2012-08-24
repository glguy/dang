{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}

module Core.Types where

import Pretty
import QualName
import TypeChecker.Vars
import Variables

import Control.Applicative ((<$>),(<*>),(*>))
import Control.Monad (guard)
import Data.Data (Data)
import Data.Serialize
    (Get,Putter,getListOf,putListOf,putWord8,getWord8,getSetOf,putSetOf)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift(..))
import qualified Data.Set as Set


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall Kind PolyFun

putScheme :: Putter Scheme
putScheme  = putForall putKind putPolyFun

getScheme :: Get Scheme
getScheme  = getForall getKind getPolyFun


-- Rank-N Functions ------------------------------------------------------------

data PolyFun = PolyFun [Scheme] Type
    deriving (Eq,Show,Ord,Data,Typeable)

instance Pretty PolyFun where
  pp p (PolyFun ps t) = optParens (p >= 1)
                      $ sep
                      $ punctuate (ppr cArrow)
                      $ map (pp 1) ps ++ [pp 1 t]

instance Lift PolyFun where
  lift (PolyFun ps t) = [| PolyFun $(lift ps) $(lift t) |]

putPolyFun :: Putter PolyFun
putPolyFun (PolyFun ps t) = putListOf putScheme ps *> putType t

getPolyFun :: Get PolyFun
getPolyFun  = PolyFun <$> getListOf getScheme <*> getType


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TCon QualName
  | TVar (TVar Kind)
    deriving (Eq,Show,Ord,Data,Typeable)

instance Pretty Type where
  pp p t = case t of
    TApp l r -> optParens (p > 0) (pp 0 l <+> pp 1 r)
    TCon qn  -> ppr qn
    TVar v   -> ppr v

instance Lift Type where
  lift t = case t of
    TApp l r -> [| TApp $(lift l) $(lift r) |]
    TCon qn  -> [| TCon $(lift qn)          |]
    TVar v   -> [| TVar $(lift v )          |]

instance FreeVars Type where
  freeVars ty = case ty of
    TApp a b -> freeVars a `Set.union` freeVars b
    TCon qn  -> Set.singleton qn
    TVar p   -> freeVars p

putType :: Putter Type
putType t = case t of
  TApp l r -> putWord8 0 *> putType l      *> putType r
  TCon qn  -> putWord8 1 *> putQualName qn
  TVar v   -> putWord8 2 *> putTVar putKind v

getType :: Get Type
getType  = do
  tag <- getWord8
  case tag of
    0 -> TApp <$> getType <*> getType
    1 -> TCon <$> getQualName
    2 -> TVar <$> getTVar getKind

-- | Type constructor for arrows.
cArrow :: QualName
cArrow  = primName ["Prelude"] "->"

tInfix :: QualName -> Type -> Type -> Type
tInfix con l r = TCon con `TApp` l `TApp` r

destInfix :: Type -> Maybe (QualName,Type,Type)
destInfix t = case t of
  TApp (TApp (TCon qn) l) r -> Just (qn,l,r)
  _                         -> Nothing

-- | Construct a function type.
tArrow :: Type -> Type -> Type
tArrow  = tInfix cArrow

destTVar :: Type -> Maybe (TVar Kind)
destTVar t = case t of
  TVar v -> Just v
  _      -> Nothing

destUVar :: Type -> Maybe (TParam Kind)
destUVar t = do
  v <- destTVar t
  case v of
    UVar p -> Just p
    _      -> Nothing

-- | Map a function over the type variables in a type
mapTVar :: (TVar Kind -> TVar Kind) -> Type -> Type
mapTVar f = loop
  where
  loop t = case t of
    TApp a b -> TApp (loop a) (loop b)
    TVar p   -> TVar (f p)
    ty       -> ty


-- Kinds -----------------------------------------------------------------------

type Kind = Type

putKind :: Putter Kind
putKind  = putType

getKind :: Get Kind
getKind  = getType


-- Class Contexts --------------------------------------------------------------

data Qual a = Qual
  { qualCxt  :: Context
  , qualData :: a
  } deriving (Show,Eq,Ord,Data,Typeable,Functor)

toQual :: a -> Qual a
toQual a = Qual emptyCxt a

putQual :: Putter a -> Putter (Qual a)
putQual p (Qual cxt a) = putContext cxt >> p a

getQual :: Get a -> Get (Qual a)
getQual m = Qual <$> getContext <*> m

instance Lift a => Lift (Qual a) where
  lift q = [| Qual
    { qualCxt  = Set.fromList $(lift (Set.toList (qualCxt q)))
    , qualData = $(lift (qualData q))
    } |]

instance Pretty a => Pretty (Qual a) where
  pp p (Qual cxt a) = optParens (p > 0) (cxtP <+> ppr a)
    where
    cxtP | Set.null cxt = empty
         | otherwise    = ppContext cxt <+> text "=>"

instance FreeVars a => FreeVars (Qual a) where
  freeVars (Qual cxt a) = freeVars cxt `Set.union` freeVars a


-- Constraints -----------------------------------------------------------------

type Constraint = Type
type Context    = Set.Set Constraint

getConstraint :: Get Constraint
getConstraint  = getType

putConstraint :: Putter Constraint
putConstraint  = putType

getContext :: Get Context
getContext  = getSetOf getConstraint

putContext :: Putter Context
putContext  = putSetOf putConstraint

emptyCxt :: Context
emptyCxt  = Set.empty

mergeCxt :: Context -> Context -> Context
mergeCxt  = Set.union

ppContext :: Context -> Doc
ppContext cxt
  | Set.null cxt = empty
  | otherwise    = parens (cat (map ppr (Set.toList cxt)))

-- | Type constructor for equality constraints.
cEq :: QualName
cEq  = primName ["Prelude"] "~"

-- | An equality constraint.
(~~) :: Type -> Type -> Constraint
(~~)  = tInfix cEq

destEq :: Constraint -> Maybe (Type,Type)
destEq c = do
  (qn,l,r) <- destInfix c
  guard (qn == cEq)
  return (l,r)
