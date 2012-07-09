{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeChecker.Subsumption where

import Core.Types (Scheme,PolyFun(..),Qual(..))
import Dang.Monad (raiseE,Exception)
import TypeChecker.Monad (TC,unify,withRigidInst,freshInst)

import Data.Typeable (Typeable)


-- Exceptions ------------------------------------------------------------------

data SubsumptionException
  = PolyFunError PolyFun PolyFun
    deriving (Show,Typeable)

instance Exception SubsumptionException

-- | The two polymorphic functions were syntactically not compatible.
polyFunError :: PolyFun -> PolyFun -> TC a
polyFunError a b = raiseE (PolyFunError a b)


-- Subsumption -----------------------------------------------------------------

class Subsumes a where
  subsumes :: Scheme -> a -> TC ()

instance Subsumes Scheme where
  subsumes s s' = withRigidInst s' (\ _ p -> subsumes s p)

-- XXX need to make sure that the contexts are compatible at this stage.
instance Subsumes (Qual PolyFun) where
  subsumes s (Qual _cxt2 p2) = do
    Qual _cxt1 p1 <- freshInst s
    subsumesPolyFun p1 p2

-- XXX not sure how to treat the context on the left at this point.
instance Subsumes PolyFun where
  subsumes s p2 = do
    Qual _cxt1 p1 <- freshInst s
    subsumesPolyFun p1 p2

-- | Subsumption between two functions of polymorphic arguments.
subsumesPolyFun :: PolyFun -> PolyFun -> TC ()
subsumesPolyFun r1@(PolyFun ps1 ty1) r2@(PolyFun ps2 ty2)
  | length ps1 /= length ps2 = polyFunError r1 r2
  | otherwise                = loop ps1 ps2
  where

  loop ls rs = case (ls,rs) of

    -- poly function
    (p1:ls',p2:rs') -> do
      subsumes p2 p1 -- note the order
      loop ls' rs'

    -- mono type
    ([],[]) -> unify ty1 ty2

    -- error, guarded by the use of `unless` above
    _ -> fail "subsumesPolyFun"
