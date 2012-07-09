{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module TypeChecker.Vars where

import Pretty
import QualName
import Variables

import Control.Applicative ((<$>),(<*>))
import Data.Data (Data)
import Data.Function (on)
import Data.Ord (comparing)
import Data.Serialize
    (Get,Putter,get,put,getWord8,putWord8,getWord32be,putWord32be,putListOf
    ,getListOf)
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax (Lift(..),liftString)
import qualified  Data.Set as Set


-- Quantifiers -----------------------------------------------------------------

-- | Things with quantified variables.
data Forall kind a = Forall
  { forallParams :: [TParam kind]
  , forallData   :: a
  } deriving (Show,Eq,Ord,Data,Typeable,Functor)

instance (Lift kind, Lift a) => Lift (Forall kind a) where
  lift qa = [| Forall
    { forallParams = $(lift (forallParams qa))
    , forallData   = $(lift (forallData qa))
    } |]

instance Pretty a => Pretty (Forall kind a) where
  pp p (Forall ps a) = optParens (p > 0 && not (null ps)) (vars <+> ppr a)
    where
    vars | null ps   = empty
         | otherwise = text "forall" <+> ppList 0 ps <> char '.'

instance FreeVars a => FreeVars (Forall kind a) where
  freeVars = freeVars  . forallData

toForall :: a -> Forall kind a
toForall  = Forall []

putForall :: Putter kind -> Putter a -> Putter (Forall kind a)
putForall putKind p (Forall ps a) = putListOf (putTParam putKind) ps >> p a

getForall :: Get kind -> Get a -> Get (Forall kind a)
getForall getKind a = Forall <$> getListOf (getTParam getKind) <*> a


-- Type Variables --------------------------------------------------------------

-- | Type variables.  One of generic, or unification.
data TVar kind
  = GVar (TParam kind)
  | UVar (TParam kind)
    deriving (Show,Eq,Ord,Data,Typeable)

instance Lift kind => Lift (TVar kind) where
  lift tv = case tv of
    UVar p -> [| UVar $(lift p) |]
    GVar p -> [| GVar $(lift p) |]

instance Pretty (TVar kind) where
  pp p (GVar v) = pp p v
  pp p (UVar v) = pp p v

instance FreeVars (TVar kind) where
  freeVars (UVar v) = Set.singleton (simpleName (paramName v))
  freeVars GVar{}   = Set.empty

putTVar :: Putter kind -> Putter (TVar kind)
putTVar putKind var = case var of
  GVar p -> putWord8 0 >> putTParam putKind p
  UVar p -> putWord8 1 >> putTParam putKind p

getTVar :: Get kind -> Get (TVar kind)
getTVar getKind = getWord8 >>= \ tag -> case tag of
  0 -> GVar <$> getTParam getKind
  1 -> UVar <$> getTParam getKind
  _ -> fail ("Invalid TVar tag: " ++ show tag)


-- Type Indices ----------------------------------------------------------------

type Index = Int

putIndex :: Putter Index
putIndex  = putWord32be . toEnum

getIndex :: Get Index
getIndex  = fromEnum <$> getWord32be


-- Type Parameters -------------------------------------------------------------

data TParam kind = TParam
  { paramIndex      :: Index
  , paramFromSource :: Bool
  , paramName       :: String
  , paramKind       :: kind
  } deriving (Show,Data,Typeable)

instance Lift kind => Lift (TParam kind) where
  lift tp = [| TParam
    { paramIndex      = $(lift       (paramIndex tp))
    , paramFromSource = $(lift       (paramFromSource tp))
    , paramName       = $(liftString (paramName tp))
    , paramKind       = $(lift       (paramKind tp))
    } |]

instance Eq (TParam kind) where
  (==) = (==) `on` paramIndex
  (/=) = (/=) `on` paramIndex

instance Ord (TParam kind) where
  compare = comparing paramIndex

instance FreeVars (TParam kind) where
  freeVars = Set.singleton . simpleName . paramName

instance Pretty (TParam kind) where
  pp _ p = text (paramName p)

modifyTParamIndex :: (Index -> Index) -> (TParam kind -> TParam kind)
modifyTParamIndex f p = p { paramIndex = f (paramIndex p) }

setTParamIndex :: Index -> TParam kind -> TParam kind
setTParamIndex ix p = p { paramIndex = ix }

putTParam :: Putter kind -> Putter (TParam kind)
putTParam putKind p = putIndex (paramIndex p)
                   >> put (paramFromSource p)
                   >> put (paramName p)
                   >> putKind (paramKind p)

getTParam :: Get kind -> Get (TParam kind)
getTParam getKind = TParam <$> getIndex <*> get <*> get <*> getKind


