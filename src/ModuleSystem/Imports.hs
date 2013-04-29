{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module ModuleSystem.Imports (
    -- * Usage Sets
    Use(..)
  , UseSet
  , UsesModules(getUses)

    -- * Import Sets
  , ImportSet
  , minimalImports
  , loadInterfaces

    -- * Errors
  , MissingInterface
  , missingInterface
  ) where

import Dang.Monad (Dang,Exception,raiseE)
import Dang.Syntax.AST
    (Module(..),Open(..),PrimType(..),PrimTerm(..),TypedDecl(..),UntypedDecl(..)
    ,DataDecl(..),ConstrGroup(..),Constr(..),Match(..),Pat(..),Term(..))
import ModuleSystem.Interface
    (readInterface,InterfaceSet,emptyInterfaceSet,addInterface)
import QualName (QualName,isSimpleName,qualModule)
import TypeChecker.Types (Forall(..),Qual(..),Type(..))

import Control.Monad (guard)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import MonadLib (BaseM,inBase,try,RunExceptionM)
import qualified Data.Set as Set
import qualified Data.Foldable as F


-- Usage Sets ------------------------------------------------------------------

data Use
  = Explicit Open     -- ^ Module use via an open declaration
  | QualTerm QualName -- ^ Module use via qualified term name
  | QualType QualName -- ^ Module use via qualified type constructor
    deriving (Eq,Show,Ord)

type UseSet = Set.Set Use

class UsesModules a where
  getUses :: a -> UseSet

instance UsesModules a => UsesModules (Maybe a) where
  getUses = maybe Set.empty getUses

instance UsesModules a => UsesModules [a] where
  getUses = Set.unions . map getUses

instance (Ord a, UsesModules a) => UsesModules (Set.Set a) where
  getUses = Set.unions . Set.toList . Set.map getUses

-- | The imports required by a module are those specified by its open
-- declarations, and implicit uses in terms or types.
instance UsesModules Module where
  getUses m = Set.unions
    [ getUses (modOpens m)
    , getUses (modTyped m)
    , getUses (modUntyped m)
    , getUses (modPrimTerms m)
    , getUses (modPrimTypes m)
    , getUses (modDatas m)
    ]

instance UsesModules Open where
  getUses = Set.singleton . Explicit

-- | Module uses from a typed declaration can arise from either its term or
-- type.  Union the two sets.
instance UsesModules TypedDecl where
  getUses td =
    getUses (typedType td) `Set.union` getUses (typedBody td)

-- | Module uses from an untyped declaration arise only from its body.
instance UsesModules UntypedDecl where
  getUses = getUses . untypedBody

-- | Module uses from a primitive term will only arise from its type schema.
instance UsesModules PrimTerm where
  getUses = getUses . primTermType

-- | Module uses from a primitive type will only arise from its kind.
instance UsesModules PrimType where
  getUses = getUses . primTypeKind

-- | Module uses from a data declaration are generated by the types referenced
-- in its constructors.
instance UsesModules DataDecl where
  getUses = getUses . dataGroups

instance UsesModules ConstrGroup where
  getUses cg = getUses (groupArgs cg) `Set.union` getUses (groupConstrs cg)

instance UsesModules Constr where
  getUses = getUses . constrFields

instance UsesModules a => UsesModules (Forall a) where
  getUses = getUses . forallData

instance UsesModules a => UsesModules (Qual a) where
  getUses (Qual cxt a) = getUses cxt `Set.union` getUses a

-- | Module uses from a type arise from the name-space used on a constructor.
instance UsesModules Type where
  getUses ty = case ty of
    TApp f x      -> getUses f `Set.union` getUses x
    TInfix qn l r -> Set.unions [globalName QualType qn, getUses l, getUses r]
    TCon qn       -> globalName QualType qn
    TVar{}        -> Set.empty

instance UsesModules Match where
  getUses (MPat p m')      = getUses p `Set.union` getUses m'
  getUses (MSplit l r)     = getUses l `Set.union` getUses r
  getUses (MTerm tm)       = getUses tm
  getUses (MGuard p e m')  = Set.unions [getUses p, getUses e, getUses m']
  getUses  MFail           = Set.empty

instance UsesModules Pat where
  getUses (PVar _)     = Set.empty
  getUses (PCon qn ps) = globalName QualTerm qn `Set.union` getUses ps
  getUses PWildcard    = Set.empty

instance UsesModules Term where
  getUses tm = case tm of
    Abs m       -> getUses m
    Case e m    -> getUses e `Set.union` getUses m
    Let ts us e -> Set.unions [getUses ts, getUses us, getUses e]
    App f xs    -> getUses f `Set.union` getUses xs
    Local _     -> Set.empty
    Global qn   -> globalName QualTerm qn
    Lit _       -> Set.empty

-- | Generate the import set for a used name, if one exists.
globalName :: (QualName -> Use) -> QualName -> UseSet
globalName k qn = fromMaybe Set.empty $ do
  guard (not (isSimpleName qn))
  return (Set.singleton (k qn))


-- Import Sets -----------------------------------------------------------------

type ImportSet = Set.Set QualName

-- | Given a set of module uses, calculate the minimal set of modules that are
-- required to satisfy the uses.
minimalImports :: UseSet -> ImportSet
minimalImports us = ms Set.\\ rs
  where
  (rs,ms)        = foldl' step (Set.empty,Set.empty) (Set.toList us)
  step (as,bs) u = case u of
    Explicit o -> ( maybe as (`Set.insert` as) (openAs o)
                  , Set.insert (openMod o) bs)
    QualTerm n -> (as,maybe bs (`Set.insert` bs) (qualModule n))
    QualType n -> (as,maybe bs (`Set.insert` bs) (qualModule n))

-- | Attempt to load an interface set that contains all the module interfaces
-- named in an import set.
loadInterfaces :: BaseM m Dang => ImportSet -> m InterfaceSet
loadInterfaces  = inBase . F.foldlM step emptyInterfaceSet
  where
  step is m = do
    e <- try (readInterface m)
    case e of
      Right iface -> return (addInterface iface is)
      Left{}      -> missingInterface m


-- Errors ----------------------------------------------------------------------

data MissingInterface = MissingInterface QualName
    deriving (Show,Typeable)

instance Exception MissingInterface

missingInterface :: BaseM m Dang => QualName -> m a
missingInterface  = inBase . raiseE . MissingInterface
