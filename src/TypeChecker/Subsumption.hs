module TypeChecker.Subsumption where

import TypeChecker.Types
import TypeChecker.Unify


-- Subsumption -----------------------------------------------------------------

instSigmaType :: Int -> SigmaType -> ([TParam],Qual RhoType)
instSigmaType ix ty = case ty of
  TScheme s ->
    let ps = [ tp { paramIndex = paramIndex tp + ix } | tp <- forallParams s ]
     in (ps,inst (map uvar ps) (forallData s))
  _         -> ([],toQual ty)


weakPrenixConversion :: SigmaType -> ([TParam],Qual RhoType)
weakPrenixConversion  = finalize . loop 0
  where

  finalize (ps,qrho) = quantifyAux 0 ps qrho

  loop ix sigma = case ty of

    -- possibly a rho type
    TInfix qn l r | qn == arrowConstr -> TInfix qn l `nest` r

    -- a monotype
    _ -> ([],qrho)
    where
    (ps,qrho)     = instSigmaType ix sigma
    ty            = qualData qrho
    nest f sigma' = (ps ++ ps', Qual cxt' ty')
      where
      (ps',qrho') = loop (ix + length ps) sigma'
      cxt'        = qualCxt qrho `mergeCxt` qualCxt qrho'
      ty'         = f (qualData qrho')
