{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE UndecidableInstances #-}

module LambdaLift (
    -- * Lambda Lifting Monad
    LL()
  , runLL
  , llDecls

    -- * Lambda Lifted AST
  , Decl(..)
  , Call(..)
  , Term(..)

    -- * Errors
  , LLError(..)
  ) where

import Error
import Pretty
import qualified AST

import Control.Applicative (Applicative(..),(<$>))
import Data.Graph (SCC(..))
import Data.Int (Int32)
import Data.List (partition,elemIndex)
import Data.Typeable (Typeable)
import MonadLib
import qualified Data.Map as Map
import qualified Data.Set as Set


type Subst = Map.Map AST.Var Term

newtype LL m a = LL
  { unLL :: ReaderT (Set.Set AST.Var) (StateT Subst (WriterT [Decl] m)) a
  } deriving (Functor,Applicative,Monad)

runLL :: ExceptionM m i => LL m a -> m (a,[Decl])
runLL (LL m) = do
  ((a,_),ds) <- runWriterT (runStateT Map.empty (runReaderT Set.empty m))
  return (a,ds)

instance BaseM m n => BaseM (LL m) n where
  inBase = LL . inBase

instance Monad m => WriterM (LL m) [Decl] where
  put = LL . put

instance Monad m => StateM (LL m) Subst where
  get = LL get
  set = LL . set

instance ExceptionM m SomeError => ExceptionM (LL m) SomeError where
  raise = LL . raise

instance RunExceptionM m SomeError => RunExceptionM (LL m) SomeError where
  try = LL . try . unLL

data LLError = LLError String
    deriving (Show, Typeable)

instance Error LLError

raiseLL :: ExceptionM m SomeError => String -> LL m a
raiseLL  = LL . raiseE . LLError

-- | Float a group of declarations out to the top-level, marking them as not
-- exported.
emits :: Monad m => [Decl] -> LL m ()
emits  = put . map notExported

extend :: Monad m => [(AST.Var,Term)] -> LL m ()
extend ns = do
  u <- get
  set $! Map.union (Map.fromList ns) u

subst :: ExceptionM m SomeError => [AST.Var] -> AST.Var -> LL m Term
subst args v = do
  u <- get
  case Map.lookup v u of
    Just t  -> return t
    Nothing ->
      case elemIndex v args of
        Just idx -> return (Argument (fromIntegral idx))
        Nothing  -> raiseLL ("Unbound variable: " ++ v)

extendVars :: [AST.Var] -> AST.Decl -> AST.Decl
extendVars vs d = d { AST.declVars = vs ++ AST.declVars d }

bindVars :: ExceptionM m SomeError => Set.Set AST.Var -> LL m a -> LL m a
bindVars vs (LL m) = LL $ do
  vs0 <- ask
  local (Set.union vs vs0) m


-- AST -------------------------------------------------------------------------

-- | Lambda-lifted declarations.  The variables only serve as documentation at
-- this point, both to describe the arity of the closure, and the names of the
-- variables used in Argument term nodes.
data Decl = Decl
  { declName     :: String
  , declExported :: Bool
  , declVars     :: [String]
  , declBody     :: Term
  } deriving Show

instance Pretty Decl where
  pp _ d =  text (declName d) <+> pp 0 (declVars d) <+> char '='
        <+> pp 0 (declBody d)

notExported :: Decl -> Decl
notExported d = d { declExported = False }

hasArguments :: Decl -> Bool
hasArguments  = not . null . declVars

data Call
  = CFun String
  | CArg Int32
    deriving Show

instance Pretty Call where
  pp p (CFun s) = optParens (p > 0) (text "alloc_closure" <+> text s)
  pp _ (CArg i) = char '$' <> ppr i

data Term
  = Apply Call [Term]
  | Let [Decl] Term
  | Symbol String
  | Argument Int32
  | Lit AST.Literal
    deriving Show

instance Pretty Term where
  pp p (Apply c ts) = optParens (p > 0)
                    $ text "apply" <+> pp 1 c <+> ppList 1 ts
  pp p (Let ds t)   =  optParens (p > 0)
                    $  text "let" <+> braces (ppList 0 ds) <+> text "in"
                   <+> pp 0 t
  pp p (Symbol s)   = optParens (p > 0) (text "alloc_closure" <+> text s)
  pp _ (Argument i) = char '$' <> ppr i
  pp _ (Lit l)      = pp 0 l


apply :: Call -> [Term] -> Term
apply (CFun s) [] = Symbol s
apply (CArg i) [] = Argument i
apply c        xs = Apply c xs


-- Lambda Lifting --------------------------------------------------------------

-- | Lambda-lift a group of declarations, checking recursive declarations in a
-- group.
llDecls :: ExceptionM m SomeError => [AST.Decl] -> LL m [Decl]
llDecls ds = concat `fmap` mapM step (AST.sccDecls ds)
  where
  names               = Set.fromList (AST.declNames ds)
  step (AcyclicSCC d) = bindVars names (createClosure [d])
  step (CyclicSCC ds) = bindVars names (createClosure ds)

-- | Rewrite references to declared names with applications that fill in its
-- free variables.  Augment the variables list for each declaration to include
-- the free variables. Descend, and lambda-lift each declaration individually.
-- It's OK to extend the arguments here, as top-level functions won't have free
-- variables.
createClosure :: ExceptionM m SomeError
              => [AST.Decl] -> LL m [Decl]
createClosure ds = do
  names <- LL ask
  let fvs = AST.freeVars ds Set.\\ names
  let fvl = Set.toList fvs
  rewriteFreeVars fvl ds
  bindVars fvs (mapM (llDecl . extendVars fvl) ds)

-- | Generate a mapping from the old symbol a declaration binds to an expression
-- that applies the free variables of that symbol.  One key assumption made here
-- is that free variables are always coming from the scope of the enclosing
-- function.
rewriteFreeVars :: ExceptionM m SomeError
                => [AST.Var] -> [AST.Decl] -> LL m ()
rewriteFreeVars fvs ds =
  extend [ (n, apply (CFun n) args) | d <- ds, let n = AST.declName d ]
  where
  args = zipWith (const . Argument) [0 ..] fvs

-- | Lambda lift the body of a declaration.  Assume that all modifications to
-- the free variables have been performed already.
llDecl :: ExceptionM m SomeError => AST.Decl -> LL m Decl
llDecl d = do
  let args = AST.declVars d
  b' <- llTerm args (AST.declBody d)
  return Decl
    { declName     = AST.declName d
    , declExported = AST.declExported d
    , declVars     = args
    , declBody     = b'
    }

-- | Lambda lift terms.  Abstractions will cause an error here, as the invariant
-- for lambda-lifting is that they have been named in a let.
llTerm :: ExceptionM m SomeError => [AST.Var] -> AST.Term -> LL m Term
llTerm args t =
  case t of
    AST.Abs{}    -> raiseLL "llTerm: unexpected Abs"
    AST.App f xs -> do
      (c,as) <- llCall args f
      xs'    <- mapM (llTerm args) xs
      return (Apply c (as ++ xs'))
    AST.Var v    -> subst args v
    AST.Lit l    -> return (Lit l)
    AST.Let ds e -> do
      ds' <- llDecls ds
      let (as,bs) = partition hasArguments ds'
      emits as
      e' <- llTerm args e
      if null bs
         then return e'
         else return (Let bs e')

llCall :: ExceptionM m SomeError
       => [AST.Var] -> AST.Term -> LL m (Call, [Term])
llCall args t = do
  let (f,xs) = AST.splitApp t
  c   <- llTerm args f
  xs' <- mapM (llTerm args) xs
  case c of
    Symbol s   -> return (CFun s, xs')
    Argument i -> return (CArg i, xs')
    Apply c as -> return (c, as ++ xs')
    _          -> raiseLL ("llCall: " ++ show c)