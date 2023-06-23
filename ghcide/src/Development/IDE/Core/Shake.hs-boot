{-# LANGUAGE DuplicateRecordFields     #-}
module Development.IDE.Core.Shake(
    ShakeOnlyExtras(..),
    IndexQueue,
    HieDbWriter(..)
    ) where

import           Control.Concurrent.STM
import           Control.Concurrent.Strict
import qualified Data.HashMap.Strict                    as HMap
import           GHC.Fingerprint
import           HieDb.Types
import           Ide.Plugin.Config
import qualified Language.LSP.Server                    as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types                     as LSP

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

-- Parts of ShakeExtras that we want to store in IdeGhcSession. This
-- cannot include parts of ShakeExtras that are defined in
-- Development.IDE.Types.Options, so that ShakeOnlyExtras can be
-- defined in Development/IDE/Core/Share.hs-boot.
data ShakeOnlyExtras = ShakeOnlyExtras
    { soeHiedbWriter :: HieDbWriter -- ^ use to write
    , soeLspEnv :: Maybe (LSP.LanguageContextEnv Config)
    }
