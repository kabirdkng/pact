{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PartialTypeSignatures #-}


module Pact.Core.Compile
 ( InterpretOutput(..)
 , interpretExprNew
 , interpretProgramNew
 , interpretProgramFileNew
 , interpretExprLisp
 , interpretProgramLisp
 , interpretProgramFileLisp
 , newInterpretBundle
 , lispInterpretBundle
--  , interpretExprTypeNew
--  , interpretExprTypeLisp
 , InterpretBundle(..)
 ) where

import Control.Lens
import Control.Monad((>=>))
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Catch
import Data.Text as Text
import Data.ByteString(ByteString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString as B

import Pact.Core.Info
import Pact.Core.Persistence
import Pact.Core.Builtin
import Pact.Core.Names
import Pact.Core.Repl.Utils
import Pact.Core.Untyped.Term
import Pact.Core.IR.Desugar

import Pact.Core.Untyped.Eval.CEK

import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Untyped.Eval.CEK as Runtime

import qualified Pact.Core.Syntax.New.Lexer as New
import qualified Pact.Core.Syntax.New.Parser as New

import qualified Pact.Core.Syntax.Lisp.Lexer as Lisp
import qualified Pact.Core.Syntax.Lisp.Parser as Lisp

data InterpretOutput b i
  = InterpretValue (CEKValue b i)
  | InterpretLog Text
  deriving Show

-- | Auxiliary type
-- to assist in swapping from the lisp frontend
data InterpretBundle
  = InterpretBundle
  { expr :: ByteString -> ReplT RawBuiltin (CEKValue RawBuiltin LineInfo)
  -- , exprType :: ByteString -> ReplT RawBuiltin (TypeScheme NamedDeBruijn)
  , program :: ByteString -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
  }

newInterpretBundle :: InterpretBundle
newInterpretBundle =
  InterpretBundle
  { expr = interpretExprNew
  -- , exprType = interpretExprTypeNew
  , program = interpretProgramNew }

lispInterpretBundle :: InterpretBundle
lispInterpretBundle =
    InterpretBundle
  { expr = interpretExprLisp
  -- , exprType = interpretExprTypeLisp
  , program = interpretProgramLisp }

interpretExprNew :: ByteString -> ReplT RawBuiltin (CEKValue RawBuiltin LineInfo)
interpretExprNew source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftIO (New.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ New.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  desugared <- liftIO (runDesugarTermNew pactdb loaded parsed)
  interpretExpr desugared

interpretExprLisp :: ByteString -> ReplT RawBuiltin (CEKValue RawBuiltin LineInfo)
interpretExprLisp source = do
  pactdb <- use replPactDb
  loaded <- use replLoaded
  lexx <- liftIO (Lisp.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ Lisp.parseExpr lexx
  debugIfFlagSet DebugParser parsed
  desugared <- liftIO (runDesugarTermLisp pactdb loaded parsed)
  interpretExpr desugared

-- interpretExpr
--   :: DesugarOutput RawBuiltin LineInfo (IR.Term IRName RawBuiltin LineInfo)
--   -> ReplT RawBuiltin (CEKValue RawBuiltin LineInfo)
-- interpretExpr (DesugarOutput desugared sup loaded' _) = do
--   (ty, typed) <- liftIO (runInferTerm sup loaded' rawBuiltinType desugared)
--   debugIfFlagSet DebugTypecheckerType ty
--   debugIfFlagSet DebugTypechecker typed
--   resolved <- liftIO (runOverloadTerm typed)
--   debugIfFlagSet DebugSpecializer resolved
--   let untyped = fromTypedTerm resolved
--   debugIfFlagSet DebugUntyped untyped
--   (value, _) <- liftIO (Runtime.runCoreCEK (_loAllLoaded loaded') undefined untyped)
--   replLoaded .= loaded'
--   pure value


interpretExpr
  :: DesugarOutput RawBuiltin LineInfo (IR.Term Name RawBuiltin LineInfo)
  -> ReplT RawBuiltin (CEKValue RawBuiltin LineInfo)
interpretExpr (DesugarOutput desugared loaded' _) = do
  let untyped = fromIRTerm desugared
  debugIfFlagSet DebugUntyped untyped
  (value, _) <- liftIO (Runtime.runRawCEK (_loAllLoaded loaded') undefined untyped)
  replLoaded .= loaded'
  pure value

-- interpretExprTypeLisp :: ByteString -> ReplT RawBuiltin (TypeScheme NamedDeBruijn)
-- interpretExprTypeLisp source = do
--   pactdb <- use replPactDb
--   loaded <- use replLoaded
--   lexx <- liftIO (Lisp.runLexerIO source)
--   debugIfFlagSet DebugLexer lexx
--   parsed <- either throwM pure $ Lisp.parseExpr lexx
--   debugIfFlagSet DebugParser parsed
--   desugared <- liftIO (runDesugarTermLisp pactdb loaded parsed)
--   interpretExprType desugared

-- interpretExprTypeNew :: ByteString -> ReplT RawBuiltin (TypeScheme NamedDeBruijn)
-- interpretExprTypeNew source = do
--   pactdb <- use replPactDb
--   loaded <- use replLoaded
--   lexx <- liftIO (New.runLexerIO source)
--   debugIfFlagSet DebugLexer lexx
--   parsed <- either throwM pure $ New.parseExpr lexx
--   debugIfFlagSet DebugParser parsed
--   desugared <- liftIO (runDesugarTermNew pactdb loaded parsed)
--   interpretExprType desugared

-- interpretExprType
--   :: DesugarOutput RawBuiltin LineInfo (IR.Term IRName RawBuiltin LineInfo)
--   -> ReplT RawBuiltin (TypeScheme NamedDeBruijn)
-- interpretExprType (DesugarOutput desugared sup loaded' _) = do
--   (ty, typed) <- liftIO (runInferTerm sup loaded' rawBuiltinType desugared)
--   debugIfFlagSet DebugTypecheckerType ty
--   debugIfFlagSet DebugTypechecker typed
--   pure ty

interpretProgramFileNew :: FilePath -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
interpretProgramFileNew source = liftIO (B.readFile source) >>= interpretProgramNew

interpretProgramNew :: ByteString -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
interpretProgramNew source = do
  loaded <- use replLoaded
  pactdb <- use replPactDb
  lexx <- liftIO (New.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ New.parseProgram lexx
  traverse (liftIO . runDesugarTopLevelNew pactdb loaded >=> interpretTopLevel) parsed

interpretProgramFileLisp :: FilePath -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
interpretProgramFileLisp source = liftIO (B.readFile source) >>= interpretProgramLisp

interpretProgramLisp :: ByteString -> ReplT RawBuiltin [InterpretOutput RawBuiltin LineInfo]
interpretProgramLisp source = do
  loaded <- use replLoaded
  pactdb <- use replPactDb
  lexx <- liftIO (Lisp.runLexerIO source)
  debugIfFlagSet DebugLexer lexx
  parsed <- either throwM pure $ Lisp.parseProgram lexx
  traverse (liftIO . runDesugarTopLevelLisp pactdb loaded >=> interpretTopLevel) parsed

-- todo: Clean up function
interpretTopLevel
  :: DesugarOutput RawBuiltin LineInfo (IR.TopLevel Name RawBuiltin LineInfo)
  -> ReplT RawBuiltin (InterpretOutput RawBuiltin LineInfo)
interpretTopLevel (DesugarOutput desugared loaded deps) = do
  p <- use replPactDb
  case fromIRTopLevel desugared of
    TLModule m -> do
      let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
          mdata = ModuleData m deps'
      liftIO (_writeModule p mdata)
      let out = "Loaded module " <> renderModuleName (_mName m)
          newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
          loaded' =
            over loModules (Map.insert (_mName m) mdata) $
            over loAllLoaded (Map.union newLoaded) loaded
      replLoaded .= loaded'
      pure (InterpretLog out)
      where
      -- Todo: remove this duplication
      -- this is a trick copied over from desugar
      toFqDep modName mhash def = let
        fqn = FullyQualifiedName modName (defName def) mhash
        in (fqn, def)

    TLTerm resolved -> do
      (value, _) <- liftIO (Runtime.runRawCEK (_loAllLoaded loaded) undefined resolved)
      replLoaded .= loaded
      pure (InterpretValue value)
    TLInterface _ -> error "interfaces not yet supported"

-- interpretTopLevel
--   :: DesugarOutput RawBuiltin LineInfo (IR.TopLevel IRName RawBuiltin LineInfo)
--   -> ReplT RawBuiltin (InterpretOutput RawBuiltin LineInfo)
-- interpretTopLevel (DesugarOutput desugared supply loaded deps) = do
--   p <- use replPactDb
--   typechecked <- liftIO (runInferTopLevel supply loaded rawBuiltinType desugared)
--   overloaded <- liftIO (runOverloadTopLevel typechecked)
--   case fromTypedTopLevel overloaded of
--     TLModule m -> do
--       let deps' = Map.filterWithKey (\k _ -> Set.member (_fqModule k) deps) (_loAllLoaded loaded)
--           mdata = ModuleData m deps'
--       liftIO (_writeModule p mdata)
--       let out = "Loaded module " <> renderModuleName (_mName m)
--           newLoaded = Map.fromList $ toFqDep (_mName m) (_mHash m) <$> _mDefs m
--           loaded' =
--             over loModules (Map.insert (_mName m) mdata) $
--             over loAllLoaded (Map.union newLoaded) loaded
--       replLoaded .= loaded'
--       pure (InterpretLog out)
--       where
--       -- Todo: remove this duplication
--       -- this is a trick copied over from desugar
--       toFqDep modName mhash def = let
--         fqn = FullyQualifiedName modName (defName def) mhash
--         in (fqn, def)

--     TLTerm resolved -> do
--       (value, _) <- liftIO (Runtime.runCoreCEK (_loAllLoaded loaded) undefined resolved)
--       replLoaded .= loaded
--       pure (InterpretValue value)
--     TLInterface _ -> error "interfaces not yet supported"
