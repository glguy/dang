module ModuleSystem.Imports where

import QualName
import Syntax.AST
import TypeChecker.Types

import Data.List (foldl')
import qualified Data.Set as Set


-- Import Sets -----------------------------------------------------------------

type ImportSet = Set.Set QualName

class HasImports a where
  getImports :: a -> ImportSet

instance HasImports a => HasImports (Maybe a) where
  getImports = maybe Set.empty getImports

instance HasImports a => HasImports [a] where
  getImports = Set.unions . map getImports

-- | The imports required by a module are those specified by its open
-- declarations, and implicit uses in terms or types.  As open declarations can
-- introduce new module names via renaming, these uses must be excluded from the
-- imports required by a module, as they only exist to it.
instance HasImports Module where
  getImports m = Set.unions
    [ ms
    , getImports (modTyped m)     Set.\\ rs
    , getImports (modUntyped m)   Set.\\ rs
    , getImports (modPrimTerms m) Set.\\ rs
    , getImports (modPrimTypes m) Set.\\ rs
    , getImports (modDatas m)     Set.\\ rs
    ]
    where
    (ms,rs) = openImports (modOpens m)


-- | Partition the imports generated by a set of open declarations into modules
-- needed, and modules generated (by a renaming import).  As module opens need
-- to be considered in both of these capacities, there is _explicitly_ no
-- instance for @HasImports@ for @Open@.
openImports :: [Open] -> (ImportSet,ImportSet)
openImports  = foldl' step (Set.empty,Set.empty)
  where
  step (ms,rs) o =
    (Set.insert (openMod o) ms, maybe rs (`Set.insert` rs) (openAs o))

-- | Module uses from a typed declaration can arise from either its term or
-- type.  Union the two sets.
instance HasImports TypedDecl where
  getImports td =
    getImports (typedType td) `Set.union` getImports (typedBody td)

-- | Module uses from an untyped declaration arise only from its body.
instance HasImports UntypedDecl where
  getImports = getImports . untypedBody

-- | Module uses from a primitive term will only arise from its type schema.
instance HasImports PrimTerm where
  getImports = getImports . primTermType

-- | Module uses from a primitive type will only arise from its kind.
instance HasImports PrimType where
  getImports = getImports . primTypeKind

-- | Module uses from a data declaration are generated by the types referenced
-- in its constructors.
instance HasImports DataDecl where
  getImports = getImports . dataConstrs

instance HasImports Constr where
  getImports = getImports . constrParams

instance HasImports a => HasImports (Forall a) where
  getImports = getImports . forallData

-- | Module uses from a type arise from the name-space used on a constructor.
instance HasImports Type where
  getImports ty = case ty of
    TApp f x      -> getImports f `Set.union` getImports x
    TInfix qn l r -> maybe id Set.insert (qualModule qn)
                   $ getImports l `Set.union` getImports r
    TCon qn       -> globalName qn
    TVar{}        -> Set.empty

instance HasImports Match where
  getImports (MPat p m') = getImports p `Set.union` getImports m'
  getImports (MTerm tm)  = getImports tm

instance HasImports Pat where
  getImports (PVar _)  = Set.empty
  getImports PWildcard = Set.empty

instance HasImports Term where
  getImports tm = case tm of
    Abs m       -> getImports m
    Let ts us e -> Set.unions [getImports ts, getImports us, getImports e]
    App f xs    -> getImports f `Set.union` getImports xs
    Local _     -> Set.empty
    Global qn   -> globalName qn
    Lit _       -> Set.empty

-- | Generate the import set for a used name, if one exists.
globalName :: QualName -> ImportSet
globalName  = maybe Set.empty Set.singleton . qualModule
