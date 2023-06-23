{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE RankNTypes                #-}
module Development.IDE.Core.Shake(
    ShakeExtras(..),
    ) where

import           Control.Concurrent.STM
import           Control.Concurrent.Strict
import           Control.Monad.Reader
import           Data.Dynamic
import           Data.EnumMap.Strict                    (EnumMap)
import           Data.Hashable
import qualified Data.HashMap.Strict                    as HMap
import           Data.Typeable
import           Development.IDE.Core.Debouncer
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.ProgressReporting
import           Development.IDE.GHC.Compat             (NameCache)
import           Development.IDE.Graph                  hiding (ShakeValue)
import           Development.IDE.Graph.Database         (ShakeDatabase)
import           Development.IDE.Types.Action
import           Development.IDE.Types.Exports
import           Development.IDE.Types.KnownTargets
import           Development.IDE.Types.Logger           hiding (Priority)
import           Development.IDE.Types.Options     (IdeTesting)
import           Development.IDE.Types.Shake
import           GHC.Fingerprint
import           HieDb.Types
import           Ide.Plugin.Config
import           Ide.Types                              (IdePlugins)
import           Language.LSP.Diagnostics
import qualified Language.LSP.Server                    as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types                     as LSP
import           Language.LSP.Types.Capabilities
import           Language.LSP.VFS                       hiding (start)
import qualified StmContainers.Map                      as STM

-- | We need to serialize writes to the database, so we send any function that
-- needs to write to the database over the channel, where it will be picked up by
-- a worker thread.
data HieDbWriter
  = HieDbWriter
  { indexQueue         :: IndexQueue
  , indexPending       :: TVar (HMap.HashMap NormalizedFilePath Fingerprint) -- ^ Avoid unnecessary/out of date indexing
  , indexCompleted     :: TVar Int -- ^ to report progress
  , indexProgressToken :: Var (Maybe LSP.ProgressToken)
  -- ^ This is a Var instead of a TVar since we need to do IO to initialise/update, so we need a lock
  }

-- | Actions to queue up on the index worker thread
-- The inner `(HieDb -> IO ()) -> IO ()` wraps `HieDb -> IO ()`
-- with (currently) retry functionality
type IndexQueue = TQueue (((HieDb -> IO ()) -> IO ()) -> IO ())

type GetStalePersistent = NormalizedFilePath -> IdeAction (Maybe (Dynamic,PositionDelta,TextDocumentVersion))

data VFSModified = VFSUnmodified | VFSModified !VFS

type STMDiagnosticStore = STM.Map NormalizedUri StoreItem

-- | A Shake database plus persistent store. Can be thought of as storing
--   mappings from @(FilePath, k)@ to @RuleResult k@.
data IdeState = IdeState
    {shakeDb              :: ShakeDatabase
    ,shakeSession         :: MVar ShakeSession
    ,shakeExtras          :: ShakeExtras
    ,shakeDatabaseProfile :: ShakeDatabase -> IO (Maybe FilePath)
    ,stopMonitoring       :: IO ()
    }

-- | A live Shake session with the ability to enqueue Actions for running.
--   Keeps the 'ShakeDatabase' open, so at most one 'ShakeSession' per database.
newtype ShakeSession = ShakeSession
  { cancelShakeSession :: IO ()
    -- ^ Closes the Shake session
  }

-- | IdeActions are used when we want to return a result immediately, even if it
-- is stale Useful for UI actions like hover, completion where we don't want to
-- block.
--
-- Run via 'runIdeAction'.
newtype IdeAction a = IdeAction { runIdeActionT  :: (ReaderT ShakeExtras IO) a }

-- information we stash inside the shakeExtra field
data ShakeExtras = ShakeExtras
    { --eventer :: LSP.FromServerMessage -> IO ()
     lspEnv :: Maybe (LSP.LanguageContextEnv Config)
    ,debouncer :: Debouncer NormalizedUri
    ,logger :: Logger
    ,idePlugins :: IdePlugins IdeState
    ,globals :: TVar (HMap.HashMap TypeRep Dynamic)
      -- ^ Registry of global state used by rules.
      -- Small and immutable after startup, so not worth using an STM.Map.
    ,state :: Values
    ,diagnostics :: STMDiagnosticStore
    ,hiddenDiagnostics :: STMDiagnosticStore
    ,publishedDiagnostics :: STM.Map NormalizedUri [Diagnostic]
    -- ^ This represents the set of diagnostics that we have published.
    -- Due to debouncing not every change might get published.
    ,positionMapping :: STM.Map NormalizedUri (EnumMap Int32 (PositionDelta, PositionMapping))
    -- ^ Map from a text document version to a PositionMapping that describes how to map
    -- positions in a version of that document to positions in the latest version
    -- First mapping is delta from previous version and second one is an
    -- accumulation of all previous mappings.
    ,progress :: ProgressReporting
    ,ideTesting :: IdeTesting
    -- ^ Whether to enable additional lsp messages used by the test suite for checking invariants
    ,restartShakeSession
        :: VFSModified
        -> String
        -> [DelayedAction ()]
        -> IO ()
#if MIN_VERSION_ghc(9,3,0)
    ,ideNc :: NameCache
#else
    ,ideNc :: IORef NameCache
#endif
    -- | A mapping of module name to known target (or candidate targets, if missing)
    ,knownTargetsVar :: TVar (Hashed KnownTargets)
    -- | A mapping of exported identifiers for local modules. Updated on kick
    ,exportsMap :: TVar ExportsMap
    -- | A work queue for actions added via 'runInShakeSession'
    ,actionQueue :: ActionQueue
    ,clientCapabilities :: ClientCapabilities
    , withHieDb :: WithHieDb -- ^ Use only to read.
    , hiedbWriter :: HieDbWriter -- ^ use to write
    , persistentKeys :: TVar (KeyMap GetStalePersistent)
      -- ^ Registry for functions that compute/get "stale" results for the rule
      -- (possibly from disk)
    , vfsVar :: TVar VFS
    -- ^ A snapshot of the current state of the virtual file system. Updated on shakeRestart
    -- VFS state is managed by LSP. However, the state according to the lsp library may be newer than the state of the current session,
    -- leaving us vulnerable to subtle race conditions. To avoid this, we take a snapshot of the state of the VFS on every
    -- restart, so that the whole session sees a single consistent view of the VFS.
    -- We don't need a STM.Map because we never update individual keys ourselves.
    , defaultConfig :: Config
      -- ^ Default HLS config, only relevant if the client does not provide any Config
    , dirtyKeys :: TVar KeySet
      -- ^ Set of dirty rule keys since the last Shake run
    }
