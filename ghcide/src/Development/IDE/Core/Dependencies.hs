module Development.IDE.Core.Dependencies
  ( indexDependencyHieFiles
  ) where

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TQueue  (writeTQueue)
import           Control.Monad                  (unless, void, when)
import           Data.Foldable                  (traverse_)
import qualified Data.Map                       as Map
import           Data.Maybe                     (isNothing)
import           Data.Set                       (Set)
import qualified Data.Set                       as Set
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Development.IDE.Core.Compile   (indexHieFile)
import           Development.IDE.Core.Rules     (HieFileCheck (..), Log,
                                                 checkHieFile)
import           Development.IDE.Core.Shake     (HieDbWriter (indexQueue),
                                                 ShakeExtras (hiedbWriter, lspEnv, withHieDb, logger))
import qualified Development.IDE.GHC.Compat     as GHC
import qualified Development.IDE.GHC.Compat.Util as GHC
import           Development.IDE.Types.Location (NormalizedFilePath,
                                                 toNormalizedFilePath')
import           HieDb                          (SourceFile (FakeFile),
                                                 lookupPackage,
                                                 removeDependencySrcFiles)
import           Ide.Logger                     (Recorder, WithPriority, logDebug)
import           Ide.Types                      (hlsDirectory)
import qualified Language.LSP.Protocol.Message  as LSP
import qualified Language.LSP.Protocol.Types    as LSP
import qualified Language.LSP.Server            as LSP
import           System.Directory               (doesDirectoryExist)
import           System.FilePath                ((<.>), (</>))

{- Note [Going to definitions in dependencies]
 - There are two main components of the functionality that enables gotoDefinition for
 - third party dependencies:
 -  + the changes to the lookupMod function in ghcide/src/Development/IDE/Core/Actions.hs,
 -    which are triggered on calls to gotoDefinition.
 -  + the code that indexes dependencies in the hiedb, which can be found in this module.
 -    This gets run asynchronously, triggering every time newHscEnvEqWithImportPaths gets called.
 -
 - The gotoDefinition code was originally written in such a way that it was
 - expecting that we would eventually be able to go to dependency definitions.
 - Before the funtionality was implemented, lookupMod was a no-op stub intended to
 - be where functionality would eventually go for dependencies. You can see the
 - code that eventually ends up calling lookupMod in the function nameToLocation in
 - ghcide/src/Development/IDE/Spans/AtPoint.hs. To summarize, gotoDefinition will look
 - for a file in the project, and look in the hiedb if it can't find it. In this sense,
 - the name lookupMod might be a little misleading, because by the time it gets called,
 - the HIE file has already been looked up in the database and we have the FilePath
 - of its location. A more appropriate name might be something like loadModule,
 - since what it does is load the module source code from an HIE file and write it out to
 - .hls/dependencies. The way nameToLocation works, if we have already opened a
 - dependency file once, lookupMod won't get called. In addition to loading the
 - dependency source and writing it out, lookupMod handles indexing the source file
 - that we wrote out, which can't happen in the initial indexing since the
 - source file doesn't exist at that point. To summarize, for gotoDefinition to work
 - for a dependency we need to have already indexed the HIE file for that dependency module.
 -
 - The indexing process gets the packages and modules for dependencies from the HscEnv.
 - It filters them for packages we know are direct or transitive dependencies, using the
 - function calculateTransitiveDependencies. indexDependencyHieFiles attempts to load an
 - HIE file for each module, checking for it in the extra-compilation-artifacts directory,
 - found in the package lib directory. This fails for the packages that ship with GHC,
 - because it doesn't yet generate HIE files. If it is able to load the HIE file,
 - it indexes it in hiedb using indexHieFile, which is the same function used to
 - index project HIE files.
 -}

newtype Package = Package GHC.UnitInfo deriving Eq
instance Ord Package where
  compare (Package u1) (Package u2) = compare (GHC.unitId u1) (GHC.unitId u2)

-- indexDependencyHieFiles gets all of the direct and transitive dependencies
-- from the HscEnv and indexes their HIE files in the HieDb.
indexDependencyHieFiles :: Recorder (WithPriority Log) -> ShakeExtras -> Bool -> GHC.HscEnv -> IO ()
indexDependencyHieFiles recorder se hasFwriteIdeInfoEnabled hscEnv = do
    -- Check whether the .hls directory exists.
    dotHlsDirExists <- maybe (pure False) doesDirectoryExist mHlsDir
    -- If the .hls directory does not exits, it may have been deleted.
    -- In this case, delete the indexed source files for all
    -- dependencies that are already indexed.
    unless dotHlsDirExists deleteMissingDependencySources
    -- Check that we are using a new enough version of cabal.
    let isUsingNewEnoughCabal = checkCabalForDependencyHieCapability se
    logDebug (logger se) $ "\n\n!!!!!!!!!!!\nIsUsingNewEnoughCabal: " <> T.pack (show isUsingNewEnoughCabal)
      <> "\nHasFwriteIdeInfoEnabled: " <> T.pack (show hasFwriteIdeInfoEnabled)
      <> "\n!!!!!!!!!!!\n"
    if isUsingNewEnoughCabal && hasFwriteIdeInfoEnabled
    then do
      let isUsingNewEnoughGhc = checkGhcForDepencencyHieCapability se
          doIndexing = void $ Map.traverseWithKey indexPackageHieFiles packagesWithModules
      if isUsingNewEnoughGhc
           -- Index all dependency HIE files in the HieDb database.
      then doIndexing
      else do
        sendWarningMessage se True incompatibleGhcWarning
        -- Index all dependency HIE files in the HieDb database.
        doIndexing
    else do
      logDebug (logger se) "\n\n!!!!!!!!!!!!\nMade it to non-indexing warning message block!\n!!!!!!!!!!\n"
      sendWarningMessage se (not isUsingNewEnoughCabal) incompatibleCabalWarning
      sendWarningMessage se (not hasFwriteIdeInfoEnabled) missingFwriteIdeInfoWarning
    where
        mHlsDir :: Maybe FilePath
        mHlsDir = do
            projectDir <- LSP.resRootPath =<< lspEnv se
            pure $ projectDir </> hlsDirectory
        -- Add the deletion of dependency source files from the
        -- HieDb database to the database write queue.
        deleteMissingDependencySources :: IO ()
        deleteMissingDependencySources =
            atomically $ writeTQueue (indexQueue $ hiedbWriter se) $
                \withHieDb ->
                    withHieDb $ \db ->
                        removeDependencySrcFiles db
        -- Index all of the modules in a package (a Unit).
        indexPackageHieFiles :: Package -> [GHC.Module] -> IO ()
        indexPackageHieFiles (Package package) modules = do
            let pkgLibDir :: FilePath
                pkgLibDir = case GHC.unitLibraryDirs package of
                  []               -> ""
                  (libraryDir : _) -> libraryDir
                -- Cabal puts the HIE files for a package in the
                -- extra-compilation-artifacts directory, provided
                -- it is compiled with the -fwrite-ide-info ghc option.
                hieDir :: FilePath
                hieDir = pkgLibDir </> "extra-compilation-artifacts"
                unit :: GHC.Unit
                unit = GHC.RealUnit $ GHC.Definite $ GHC.unitId package
            -- Check if we have already indexed this package.
            moduleRows <- withHieDb se $ \db ->
                lookupPackage db unit
            case moduleRows of
                -- There are no modules from this package in the database,
                -- so go ahead and index all the modules.
                [] -> traverse_ (indexModuleHieFile hieDir) modules
                -- There are modules from this package in the database,
                -- so assume all the modules have already been indexed
                -- and do nothing.
                _  -> return ()
        indexModuleHieFile :: FilePath -> GHC.Module -> IO ()
        indexModuleHieFile hieDir m = do
            let hiePath :: NormalizedFilePath
                hiePath = toNormalizedFilePath' $
                  hieDir </> GHC.moduleNameSlashes (GHC.moduleName m) <.> "hie"
            -- Check that the module HIE file has correctly loaded. If there
            -- was some problem loading it, or if it has already been indexed
            -- (which shouldn't happen because we check whether each package
            -- has been indexed), then do nothing. Otherwise, call the
            -- indexHieFile function from Core.Compile.
            hieCheck <- checkHieFile recorder se "newHscEnvEqWithImportPaths" hiePath
            case hieCheck of
                HieFileMissing -> return ()
                HieAlreadyIndexed -> return ()
                CouldNotLoadHie _e -> return ()
                DoIndexing hash hie ->
                    -- At this point there is no source file for the HIE file,
                    -- so the HieDb.SourceFile we give is FakeFile Nothing.
                    indexHieFile se hiePath (FakeFile Nothing) hash hie
        packagesWithModules :: Map.Map Package [GHC.Module]
        packagesWithModules = Map.fromSet getModulesForPackage packages
        packages :: Set Package
        packages = Set.fromList
            $ map Package
            $ Map.elems
            -- Take only the packages in the unitInfoMap that are direct
            -- or transitive dependencies.
            $ Map.filterWithKey (\uid _ -> uid `Set.member` dependencyIds) unitInfoMap
            where
                unitInfoMap :: GHC.UnitInfoMap
                unitInfoMap = GHC.getUnitInfoMap hscEnv
                dependencyIds :: Set GHC.UnitId
                dependencyIds =
                    calculateTransitiveDependencies unitInfoMap directDependencyIds directDependencyIds
                directDependencyIds :: Set GHC.UnitId
                directDependencyIds = Set.fromList
                    $ map GHC.toUnitId
                    $ GHC.explicitUnits
                    $ GHC.unitState hscEnv

checkCabalForDependencyHieCapability :: ShakeExtras -> Bool
checkCabalForDependencyHieCapability = const True

checkGhcForDepencencyHieCapability :: ShakeExtras -> Bool
checkGhcForDepencencyHieCapability = const False

sendWarningMessage
  :: ShakeExtras
  -> Bool
  -> Text
  -> IO ()
sendWarningMessage se shouldSend warning = case lspEnv se of
  Nothing -> logDebug (logger se) "\n\n!!!!!!!!!!!!\nNoLspEnv\n!!!!!!!!!!\n"
  Just env -> when shouldSend $ do
    logDebug (logger se) $ "\n\n!!!!!!!!!!!\nSending Warning: "
      <> warning <> "\n!!!!!!!!!!\n"
    LSP.runLspT env $
      LSP.sendNotification LSP.SMethod_WindowShowMessage $
        LSP.ShowMessageParams LSP.MessageType_Warning warning

incompatibleCabalWarning :: Text
incompatibleCabalWarning = T.unwords
  [ "Goto definition will not work for dependencies with this version of cabal-install."
  , "Please ensure that you are using cabal-install version 3.11 or later"
  , "and restart the language server."
  ]

missingFwriteIdeInfoWarning :: Text
missingFwriteIdeInfoWarning = T.unwords
  [ "Goto definition will not work for dependencies without the -fwrite-ide-info ghc flag."
  , "Please enable ghc-options: -fwrite-ide-info in your cabal.project"
  , "or in your global cabal/config and restart the language server."
  ]

incompatibleGhcWarning :: Text
incompatibleGhcWarning = T.unwords
  -- We should update this message when a version of GHC ships
  -- that supports distributing .hie files with the packages
  -- GHC ships with.
  [ "Goto definition will not work for the dependencies that ship with GHC."
  , "Goto definition is expected to be supported for these dependencies"
  , "starting with GHC 9.10."
  ]

-- calculateTransitiveDependencies finds the UnitId keys in the UnitInfoMap
-- that are dependencies or transitive dependencies.
calculateTransitiveDependencies :: GHC.UnitInfoMap -> Set GHC.UnitId -> Set GHC.UnitId -> Set GHC.UnitId
calculateTransitiveDependencies unitInfoMap allDependencies newDepencencies
    -- If there are no new dependencies, we have found them all,
    -- so return allDependencies
    | Set.null newDepencencies = allDependencies
    -- Otherwise recursively add any dependencies of the newDepencencies
    -- that are not in allDependencies already.
    | otherwise = calculateTransitiveDependencies unitInfoMap nextAll nextNew
    where
        nextAll :: Set GHC.UnitId
        nextAll = Set.union allDependencies nextNew
        -- Get the dependencies of the newDependencies. Then the nextNew depencencies
        -- will be the set difference of the dependencies we have so far (allDependencies),
        -- and the dependencies of the newDepencencies.
        nextNew :: Set GHC.UnitId
        nextNew = flip Set.difference allDependencies
            $ Set.unions
            $ map (Set.fromList . GHC.unitDepends)
            $ Map.elems
            $ Map.filterWithKey (\uid _ -> uid `Set.member` newDepencencies) unitInfoMap

getModulesForPackage :: Package -> [GHC.Module]
getModulesForPackage (Package package) =
    map makeModule allModules
    where
        allModules :: [GHC.ModuleName]
        allModules = map fst
            -- The modules with a Just value in the tuple
            -- are from other packages. These won't have
            -- an HIE file in this package, and should be
            -- covered by the transitive dependencies.
            ( filter (isNothing . snd)
            $ GHC.unitExposedModules package
            )
            ++ GHC.unitHiddenModules package
        makeModule :: GHC.ModuleName
                   -> GHC.Module
        makeModule = GHC.mkModule (GHC.mkUnit package)
