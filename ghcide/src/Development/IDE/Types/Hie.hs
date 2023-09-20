module Hie
  ( HieFileStatus
  , HieLoadError
  , getHieFileStatus
  ) where

-- | An error loading an Hie file that we expect
-- to exist.
data HieLoadError
  -- | The Hie file we want to load is missing.
  = HieFileMissing
  -- | The Hie file exists but an exception
  -- was thrown while attempting to load it.
  | CouldNotLoadHie SomeException

-- | The status of an Hie file we are trying
-- to index.
data HieFileStatus
  -- | The Hie file has already been indexed, so
  -- we don't need to do anything.
  = HieAlreadyIndexed
  -- | The Hie file has not been indexed, and
  -- was successfully loaded with everything we
  -- need to index it, including its Fingerprint
  -- and the HieFile data.
  | HieNotYetIndexed Util.Fingerprint HieFile

-- checkHieFile verifies that an HIE file exists, that it has not already
-- been indexed, and attempts to load it. This is intended to happen before
-- any indexing of HIE files in the HieDb database. In addition to returning
-- a HieFileCheck, this function also handles logging.
getHieFileStatus
  :: Recorder (WithPriority Log)
  -> ShakeExtras
  -> String
  -> NormalizedFilePath
  -> ExceptT HieLoadError IO HieFileStatus
getHieFileStatus recorder se@ShakeExtras{withHieDb} tag hieFileLocation = do
  hieFileExists <- doesFileExist $ fromNormalizedFilePath hieFileLocation
  if hieFileExists
  then checkExistingHieFile
  else logHieFileMissing
  where
    -- Log that the HIE file does not exist where we expect that it should.
    logHieFileMissing :: ExceptT HieLoadError IO HieFileStatus
    logHieFileMissing = do
      let logMissing :: Log
          logMissing = LogMissingHieFile hieFileLocation
      logWith recorder Logger.Debug logMissing
      throw HieFileMissing
    -- When we know that the HIE file exists, check that it has not already
    -- been indexed. If it hasn't, try to load it.
    checkExistingHieFile :: ExceptT HieLoadError IO HieFileStatus
    checkExistingHieFile = do
      hieFileHash <- Util.getFileHash $ fromNormalizedFilePath hieFileLocation
      mrow <- withHieDb (\hieDb -> HieDb.lookupHieFileFromHash hieDb hieFileHash)
      dbHieFileLocation <- traverse (makeAbsolute . HieDb.hieModuleHieFile) mrow
      if Just hieFileLocation == fmap toNormalizedFilePath' dbHieFileLocation
      -- We got a result from the database corresponding to the hash
      -- of the Hie file we are checking for, and with a matching
      -- NormalizedFilePath for the location of the Hie file, so we
      -- know that this file has already been indexed in the
      -- database.
      then pure HieAlreadyIndexed
      -- The file Fingerprint was either not found in the database,
      -- or the location didn't match, so our Hie file has not been
      -- indexed. We will try to load the Hie file for indexing.
      else tryLoadingHieFile hieFileHash

    -- Attempt to load the HIE file, logging on failure (logging happens
    -- in readHieFileFromDisk). If the file loads successfully, return
    -- the data necessary for indexing it in the HieDb database.
    tryLoadingHieFile :: Util.Fingerprint
                      -> ExceptT HieLoadError IO HieFileStatus
    tryLoadingHieFile hieFileHash = do
      ehf <- runIdeAction tag se $ runExceptT $
        readHieFileFromDisk recorder hieFileLocation
      pure $ case ehf of
        Left err -> throw $ CouldNotLoadHie err
        Right hf -> pure $ HieNotYetIndexed hieFileHash hf
