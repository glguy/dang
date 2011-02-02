module Compile where

import CodeGen
import Dang.IO
import Dang.Monad
import Dang.Tool
import Interface
import LambdaLift
import Pretty
import QualName
import ReadWrite
import Rename
import qualified Syntax.AST as AST

import Text.LLVM
import MonadLib
import System.IO (hPrint,hFlush)

compile :: Interface R -> AST.Module -> FilePath -> Dang ()
compile iface m out = do
  let m' = rename m
  logDebug "Renaming output:"
  logDebug (show m')

  decls <- lambdaLift iface m'
  logDebug "Lambda-lifting output"
  logDebug (show decls)
  logDebug (unlines ["Lambda-lifted decls:", pretty decls])

  withOpenTempFile $ \ llvm h -> do
    doc <- codeGen (AST.modName m) iface decls
    io $ do
      hPrint h doc
      hFlush h
    withClosedTempFile $ \ bc -> do
      sync llvm_as ["-o", bc, llvm]
      withClosedTempFile $ \ asm -> do
        sync llc       ["-o", asm, bc]
        sync assembler ["-o", out, asm]

rename :: AST.Module -> AST.Module
rename  = runLift . runRename [] . renameModule

lambdaLift :: Interface R -> AST.Module -> Dang [Decl]
lambdaLift iface m = do
  (as,bs) <- runLL iface (llModule m)
  return (as ++ bs)

codeGen :: QualName -> Interface R -> [Decl] -> Dang Doc
codeGen qn env ds = do
  writeInterface qn $! iface
  return doc
  where
  (iface,doc) = runLLVM (rtsImports >> compModule env ds)