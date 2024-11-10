{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Pact.Repl.Types
    ( -- * Core REPL Types
      ReplMode(..)
    , Hdl(..)
    , ReplState(..)
    -- * Display and Styling
    , DisplayOptions(..)
    , StyleOptions(..)
    -- * State Lenses
    , rEnv
    , rEvalState
    , rMode
    , rOut
    , rFile
    , rTermOut
    , rDisplayOptions
    , rStyle
    -- * Testing Types
    , TestResult(..)
    -- * REPL Monad
    , Repl
    -- * Library Types
    , LibOp(..)
    , LibState(..)
    , LibDb(..)
    , Tx(..)
    , SPVMockKey(..)
    -- * Module Operations
    , getAllModules
    -- * Display Options Lenses
    , doColor
    , doPromptStyle
    , doShowTypes
    , doDebugOutput
    -- * Style Options Lenses
    , soBold
    , soItalic
    , soUnderline
    ) where

import Control.Lens (makeLenses)
import Control.Monad
import Control.Monad.State.Strict (StateT)
import Control.Concurrent (MVar)
import Data.Default (Default(..))
import Data.Monoid (Endo(..))
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..))
import GHC.Generics

import Pact.PersistPactDb (DbEnv)
import Pact.Runtime.Utils
import Pact.Types.Runtime
import Pact.Types.Pretty (Pretty, pretty, renderCompactText)
import Pact.Types.Typecheck

-- | Style configuration for terminal output
data StyleOptions = StyleOptions
    { _soBold      :: !Bool -- ^ Enable bold text
    , _soItalic    :: !Bool -- ^ Enable italic text
    , _soUnderline :: !Bool -- ^ Enable underlined text
    } deriving (Show, Eq, Generic)

instance Default StyleOptions where
    def = StyleOptions True False False

instance ToJSON StyleOptions
instance FromJSON StyleOptions

-- | Display configuration for REPL
data DisplayOptions = DisplayOptions
    { _doColor       :: !Bool  -- ^ Enable colored output
    , _doPromptStyle :: !Text  -- ^ REPL prompt style
    , _doShowTypes   :: !Bool  -- ^ Show type information
    , _doDebugOutput :: !Bool  -- ^ Show debug information
    } deriving (Show, Eq, Generic)

instance Default DisplayOptions where
    def = DisplayOptions True "pact> " True False

instance ToJSON DisplayOptions
instance FromJSON DisplayOptions

-- | REPL execution modes
data ReplMode
    = Interactive              -- ^ Interactive REPL mode
    | Script
        { rmTrace :: !Bool     -- ^ Enable trace output
        , rmFile  :: !FilePath -- ^ Script file path
        }
    | FailureTest             -- ^ Test mode expecting failures
    | Quiet                   -- ^ Suppress output
    | StringEval              -- ^ String evaluation mode
    | StdinPipe              -- ^ Reading from stdin
    deriving (Eq, Show)

-- | Output handle specification
data Hdl = HOut | HErr
    deriving (Eq, Show)

-- | Test execution result
data TestResult = TestResult
    { trName    :: !Text                    -- ^ Test name
    , trFailure :: !(Maybe (FunApp, Text))  -- ^ Optional failure info
    } deriving (Show)

-- | Complete REPL state
data ReplState = ReplState
    { _rEnv            :: !(EvalEnv LibState)    -- ^ Evaluation environment
    , _rEvalState      :: !EvalState             -- ^ Current evaluation state
    , _rMode           :: !ReplMode              -- ^ Current REPL mode
    , _rOut            :: !String                -- ^ Accumulated output
    , _rTermOut        :: ![Term Name]           -- ^ Term output history
    , _rFile           :: !(Maybe FilePath)      -- ^ Current file being processed
    , _rDisplayOptions :: !DisplayOptions        -- ^ Display configuration
    , _rStyle          :: !StyleOptions          -- ^ Style configuration
    }

-- | REPL monad type
type Repl a = StateT ReplState IO a

-- | Operations requiring top-level evaluation
data LibOp
    = Noop                                      -- ^ No operation
    | UpdateEnv !(Endo (EvalEnv LibState))      -- ^ Update environment
    | Load !FilePath !Bool                      -- ^ Load file with reset flag
    | Print !(Term Name)                        -- ^ Print term
    | Output ![RenderedOutput]                  -- ^ Special output
    deriving (Show)

instance Default LibOp where
    def = Noop

instance Semigroup LibOp where
    UpdateEnv a <> UpdateEnv b = UpdateEnv (a <> b)
    _ <> b = b

instance Monoid LibOp where
    mempty = def

-- | Transaction types
data Tx = Begin | Commit | Rollback
    deriving (Eq, Show, Bounded, Enum, Ord)

-- | Key type for SPV mocking
newtype SPVMockKey = SPVMockKey (Text, Object Name)
    deriving (Show)

instance Pretty SPVMockKey where
    pretty (SPVMockKey (t,o)) = pretty t <> pretty o

instance Eq SPVMockKey where
    a == b = renderCompactText a == renderCompactText b

instance Ord SPVMockKey where
    a `compare` b = renderCompactText a `compare` renderCompactText b

-- | Database abstraction
data LibDb = forall e. LibDb (MVar (DbEnv e))

-- | Library state
data LibState = LibState
    { _rlsDb        :: !LibDb                           -- ^ Database handle
    , _rlsOp        :: !LibOp                           -- ^ Current operation
    , _rlsTx        :: !(Maybe TxId, Maybe Text)        -- ^ Transaction state
    , _rlsTests     :: ![TestResult]                    -- ^ Test results
    , _rlsVerifyUri :: !(Maybe String)                  -- ^ Verification URI
    , _rlsMockSPV   :: !(M.Map SPVMockKey (Object Name)) -- ^ Mock SPV data
    , _rlsDynEnv    :: !DynEnv                         -- ^ Dynamic environment
    }

-- Generate lenses
makeLenses ''LibState
makeLenses ''ReplState
makeLenses ''DisplayOptions
makeLenses ''StyleOptions

-- | Get all loaded modules
getAllModules :: HasInfo i => i -> Eval e (HM.HashMap ModuleName (ModuleData Ref))
getAllModules i = do
    mks <- keys (getInfo i) Modules
    fmap HM.fromList $ forM mks $ \mk -> do
        m <- getModule i mk
        return (mk, m)

-- | Helper functions for display configuration
isColorEnabled :: ReplState -> Bool
isColorEnabled = _doColor . _rDisplayOptions

getPromptStyle :: ReplState -> Text
getPromptStyle = _doPromptStyle . _rDisplayOptions

shouldShowTypes :: ReplState -> Bool
shouldShowTypes = _doShowTypes . _rDisplayOptions

isDebugEnabled :: ReplState -> Bool
isDebugEnabled = _doDebugOutput . _rDisplayOptions

-- | Style helper functions
isBoldEnabled :: ReplState -> Bool
isBoldEnabled = _soBold . _rStyle

isItalicEnabled :: ReplState -> Bool
isItalicEnabled = _soItalic . _rStyle

isUnderlineEnabled :: ReplState -> Bool
isUnderlineEnabled = _soUnderline . _rStyle
