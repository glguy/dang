module ModuleSystem.Resolve (

    -- * Resolved Names
    ResolvedNames
  , mergeResolvedNames
  , Resolved(..), isBound, shadows
  , mergeResolved

    -- * Top-level Names
  , modResolvedNames

    -- * Usage Resolution
  , resolveUses
  ) where

import Interface (IsInterface)
import ModuleSystem.Imports (UseSet)
import QualName (QualName,Name,Namespace,qualName,primName,simpleName)
import Syntax.AST
    (Module(..),modNamespace,PrimType(..),PrimTerm(..),UntypedDecl(..)
    ,TypedDecl(..),DataDecl(..),Constr(..))
import TypeChecker.Types (forallData)
import qualified Data.ClashMap as CM


-- Name Resolution -------------------------------------------------------------

-- | A clash-map from names appearing in the source to names resolved by the
-- import environment.
type ResolvedNames = CM.ClashMap UsedName Resolved

-- | Lexical scope numbering.
type Level = Int

data UsedName
  = UsedType QualName
  | UsedTerm QualName
    deriving (Ord,Show,Eq)

-- | Names resolved by an interface, or function environment.
data Resolved
  = Resolved Level QualName -- ^ A resolved, top-level name
  | Bound Name              -- ^ A resolved bound name
    deriving (Eq,Show)

-- | True when the resolved name is a bound variable.
isBound :: Resolved -> Bool
isBound Bound{} = True
isBound _       = False

-- | True when the left argument shadows the right argument by belonging to a
-- deeper scope.
shadows :: Resolved -> Resolved -> Bool
shadows (Resolved i _) (Resolved j _) = i > j
shadows _              _              = False

-- | Merge resolved names.
mergeResolved :: CM.Strategy Resolved
mergeResolved a b
  | isBound a     = CM.ok a
  | a == b        = CM.ok a
  | a `shadows` b = CM.ok a
  | otherwise     = CM.clash a b

-- | Merge two name resolution maps, allowing for shadowing.  The left
-- substitution will take precedence over the right one.
mergeResolvedNames :: ResolvedNames -> ResolvedNames -> ResolvedNames
mergeResolvedNames  = CM.unionWith mergeResolved


-- Defined Names ---------------------------------------------------------------

-- | Generate the set of resolved names that a module defines at the top-level.
modResolvedNames :: Module -> ResolvedNames
modResolvedNames m = CM.fromList
                   $ concatMap (dataDeclNames ns) (modDatas m)
                  ++ concatMap (primTypeNames ns) (modPrimTypes m)
                  ++ concatMap (primTermNames ns) (modPrimTerms m)
                  ++ concatMap (typedNames ns)    (modTyped m)
                  ++ concatMap (untypedNames ns)  (modUntyped m)
  where
  ns = modNamespace m

-- | Extract the simple and qualified names that a declaration introduces.
declResolvedNames :: (a -> Name) -> (QualName -> UsedName) -> (Name -> QualName)
                  -> (a -> [(UsedName,Resolved)])
declResolvedNames simple k qualify d = [ (k (simpleName n), res), (k qn, res) ]
  where
  n   = simple d
  qn  = qualify n
  res = Resolved 0 qn

dataDeclNames :: Namespace -> DataDecl -> [(UsedName,Resolved)]
dataDeclNames ns d = declResolvedNames dataName UsedType (qualName ns) d
                  ++ concatMap (constrNames ns) (forallData (dataConstrs d))

constrNames :: Namespace -> Constr -> [(UsedName,Resolved)]
constrNames  = declResolvedNames constrName UsedTerm . qualName

-- | The resolved names from a single typed declaration.
typedNames :: Namespace -> TypedDecl -> [(UsedName,Resolved)]
typedNames  = declResolvedNames typedName UsedTerm . qualName

untypedNames :: Namespace -> UntypedDecl -> [(UsedName,Resolved)]
untypedNames  = declResolvedNames untypedName UsedTerm . qualName

-- | The resolved names form a single primitive type declaration.
primTypeNames :: Namespace -> PrimType -> [(UsedName,Resolved)]
primTypeNames  = declResolvedNames primTypeName UsedType . primName

-- | The resolved names form a single primitive term declaration.
primTermNames :: Namespace -> PrimTerm -> [(UsedName,Resolved)]
primTermNames  = declResolvedNames primTermName UsedTerm . primName


-- UseSet Interface ------------------------------------------------------------

resolveUses :: IsInterface iface => iface -> UseSet -> ResolvedNames
resolveUses iface = undefined
