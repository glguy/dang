{-# LANGUAGE DeriveDataTypeable #-}

module Core.Types where

import QualName
import TypeChecker.Vars

import Data.Data (Data)
import Data.Typeable (Typeable)


-- Type Schemes ----------------------------------------------------------------

type Scheme = Forall Kind PolyFun


-- Rank-N Functions ------------------------------------------------------------

data PolyFun = PolyFun [Scheme] Type
    deriving (Eq,Show,Ord,Data,Typeable)


-- Types -----------------------------------------------------------------------

data Type
  = TApp Type Type
  | TCon QualName
  | TVar (TVar Kind)
    deriving (Eq,Show,Ord,Data,Typeable)


-- Kinds -----------------------------------------------------------------------

type Kind = Type
