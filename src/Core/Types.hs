{-# LANGUAGE DeriveDataTypeable #-}

module Core.Types where

import TypeChecker.Vars

import Data.Data (Data)
import Data.Typeable (Typeable)


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall PolyFun


-- Rank-N Functions ------------------------------------------------------------

data PolyFun = PolyFun [Scheme] Type
    deriving (Eq,Show,Ord,Data,Typeable)


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TCon Qualname
  | TVar TVar
    deriving (Eq,Show,Ord,Data,Typeable)
