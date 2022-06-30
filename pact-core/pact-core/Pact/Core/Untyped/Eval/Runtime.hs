{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Pact.Core.Untyped.Eval.Runtime
 ( CEKTLEnv
 , CEKEnv
 , HasTLEnv
 , HasBuiltinEnv
 , HasRuntimeEnv
 , CEKRuntime
 , BuiltinFn(..)
 , CEKState(..)
 , EvalT(..)
 , runEvalT
 , CEKValue(..)
 , Cont(..)
 , RuntimeEnv(..)
 , ckeData
 , ckeTxHash
 , ckeResolveName
 ) where


import Control.Lens
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Text(Text)
import Data.Map.Strict(Map)
import Data.Vector(Vector)
-- import Data.List.NonEmpty(NonEmpty(..))
import Data.RAList(RAList)
import Data.Primitive(Array)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Pretty(Pretty(..), (<+>))
import Pact.Core.Gas
import Pact.Core.PactValue
import Pact.Core.Hash
import Pact.Core.Untyped.Term
import Pact.Core.Literal
import qualified Pact.Core.Pretty as P

-- | The top level env map
type CEKTLEnv b i = Map FullyQualifiedName (EvalTerm b i)
-- | Locally bound variables
type CEKEnv b i = RAList (CEKValue b i)

-- | Top level constraint
type HasTLEnv b i = (?cekLoaded :: CEKTLEnv b i)
-- | List of builtins
type HasBuiltinEnv b i = (?cekBuiltins :: Array (BuiltinFn b i))
-- | runtime env
type HasRuntimeEnv b i = (?cekRuntimeEnv :: RuntimeEnv b i)

type CEKRuntime b i = (HasTLEnv b i, HasBuiltinEnv b i, HasRuntimeEnv b i, Enum b)

-- | The type of our semantic runtime values
data CEKValue b i
  = VLiteral !Literal
  | VObject !(Map Field (CEKValue b i))
  | VList !(Vector (CEKValue b i))
  | VClosure !(EvalTerm b i) !(CEKEnv b i)
  | VNative !(BuiltinFn b i)
  | VGuard !(Guard Name (CEKValue b i))
  deriving (Show)

data CEKState b
  = CEKState
  { _cekGas :: Gas
  , _cekEvalLog :: Maybe [Text]
  } deriving Show

newtype EvalT b a =
  EvalT (StateT (CEKState b) IO a)
  deriving
    ( Functor, Applicative, Monad
    , MonadState (CEKState b)
    , MonadIO
    , MonadThrow
    , MonadCatch)
  via (StateT (CEKState b) IO)

runEvalT :: CEKState b -> EvalT b a -> IO (a, CEKState b)
runEvalT s (EvalT action) = runStateT action s

data BuiltinFn b i
  = BuiltinFn
  { _native :: b
  , _nativeFn :: CEKRuntime b i => [CEKValue b i] -> EvalT b (CEKValue b i)
  , _nativeArity :: {-# UNPACK #-} !Int
  , _nativeAppliedArgs :: [CEKValue b i]
  }

data ExecutionMode
  = Transactional
  | Local
  deriving (Eq, Show, Bounded, Enum)

data Cont b i
  = Fn (CEKValue b i) (Cont b i)
  | Arg (CEKEnv b i) (EvalTerm b i) (Cont b i)
  | BlockC (CEKEnv b i) [EvalTerm b i] (Cont b i)
  | Mt
  deriving Show

data RuntimeEnv b i
  = RuntimeEnv
  { _ckeData :: EnvData (EvalTerm b i)
  , _ckeTxHash :: Hash
  , _ckeResolveName :: QualifiedName -> EvalT b FullyQualifiedName
  }

instance (Pretty b) => Show (BuiltinFn b i) where
  show (BuiltinFn b _ arity args) = unwords
    ["(BuiltinFn"
    , show (pretty b)
    , "#fn"
    , show arity
    , show (pretty args)
    , ")"
    ]

instance (Pretty b) => Pretty (BuiltinFn b i) where
  pretty = pretty . show

instance Pretty b => Pretty (CEKValue b i) where
  pretty = \case
    VLiteral i -> pretty i
    VObject o ->
      let toBind (k, e) = pretty k <> ":" <> pretty e
      in P.braces $ P.hsep (P.punctuate P.comma (toBind <$> Map.toList o))
    VList v ->
      P.brackets $ P.hsep (P.punctuate P.comma (V.toList (pretty <$> v)))
    VClosure{} ->
      P.angles "closure#"
    VNative b ->
      P.angles $ "native" <+> pretty b
    VGuard _ -> error "undefined"

makeLenses ''RuntimeEnv