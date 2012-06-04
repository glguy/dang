{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.Subsumption where

import Dang.IO (logDebug,logInfo)
import Dang.Monad (raiseE,Exception)
import TypeChecker.Monad (TC,unify,withRigidInst)
import TypeChecker.Types (Scheme,PolyFun(..),Type,Qual(..))

import Data.Typeable (Typeable)


-- Exceptions ------------------------------------------------------------------

data SubsumptionException
  = PolyFunError (Qual PolyFun) (Qual PolyFun)
    deriving (Show,Typeable)

instance Exception SubsumptionException

-- | The two polymorphic functions were syntactically not compatible.
polyFunError :: Qual PolyFun -> Qual PolyFun -> TC a
polyFunError a b = raiseE (PolyFunError a b)


-- Subsumption -----------------------------------------------------------------

-- | Subsumption.
subsumes :: Scheme -> Scheme -> TC ()
subsumes s1 s2 = do
  logInfo "before"
  withRigidInst s1 $ \ _ p1 -> do
   logInfo "before 2"
   withRigidInst s2 $ \ _ p2 -> do
    logInfo "polyfun"
    subsumesPolyFun p1 p2

-- | Subsumption between two functions of polymorphic arguments.
--
-- XXX need to make sure that the contexts are compatible here.
subsumesPolyFun :: Qual PolyFun -> Qual PolyFun -> TC ()
subsumesPolyFun qp1 qp2
  | length ps1 /= length ps2 = polyFunError qp1 qp2
  | otherwise                = loop ps1 ps2
  where
  PolyFun ps1 ty1 = qualData qp1
  PolyFun ps2 ty2 = qualData qp2

  loop ls rs = case (ls,rs) of

    -- poly function
    (p1:ls',p2:rs') -> do
      subsumes p2 p1 -- note the order
      loop ls' rs'

    -- mono type
    ([],[]) -> subsumesType ty1 ty2

    -- error, guarded by the use of `unless` above
    _ -> fail "subsumesPolyFun"

-- | Subsumption between two mono-types.
subsumesType :: Type -> Type -> TC ()
subsumesType ty1 ty2 = unify ty1 ty2
