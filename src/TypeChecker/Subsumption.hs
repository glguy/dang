{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TypeChecker.Subsumption where

import Dang.Monad (raiseE,Exception)
import TypeChecker.Monad (TC,unify,withRigidInst,freshInst)
import TypeChecker.Types (Scheme,PolyFun(..),Qual(..))

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
subsumes s1 s2 =
  withRigidInst s2 $ \ _ p2 -> do
    subsumesPolyFun s1 p2

-- | Subsumption between two functions of polymorphic arguments.
--
subsumesPolyFun :: Scheme -> Qual PolyFun -> TC ()
subsumesPolyFun s1 qp2 = do

  qp1 <- freshInst s1

  -- XXX need to make sure that the contexts are compatible here.
  let PolyFun ps1 ty1 = qualData qp1
      PolyFun ps2 ty2 = qualData qp2

      loop ls rs = case (ls,rs) of

        -- poly function
        (p1:ls',p2:rs') -> do
          subsumes p2 p1 -- note the order
          loop ls' rs'

        -- mono type
        ([],[]) -> unify ty1 ty2

        -- error, guarded by the use of `unless` above
        _ -> fail "subsumesPolyFun"

  if length ps1 /= length ps2
     then polyFunError qp1 qp2
     else loop ps1 ps2
