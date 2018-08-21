
module DDC.Driver.Build.Run where
import DDC.Driver.Build.Query
import DDC.Driver.Build.State
import DDC.Driver.Interface.Source
import DDC.Driver.Config
import qualified DDC.Driver.Interface.Status            as Status

import Control.DeepSeq
import Data.Maybe

import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Interface.Store               as C
import qualified DDC.Core.Transform.Reannotate          as C.Reannotate
import qualified DDC.Core.Codec.Shimmer.Encode          as C.Encode

import qualified DDC.Driver.Stage.Tetra                 as G.Discus
import qualified DDC.Driver.Stage.Salt                  as G.Salt

import qualified DDC.Core.Discus                        as D
import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as D.Encode

import qualified DDC.Core.Salt                          as S

import qualified DDC.Version                            as V
import qualified DDC.Core.Salt.Runtime                  as A

import qualified System.FilePath                        as S
import qualified System.Directory                       as S
import qualified Data.Time.Clock                        as S
import qualified Data.Text                              as T
import qualified Data.Set                               as Set


---------------------------------------------------------------------------------------------------
-- | Try to run a job, returning True if it made progress.
runJob :: State -> Job -> S Bool
runJob state job
 = case job of
         JobBuildModuleOfName nModule
          -> runBuildModuleOfName state nModule

         JobBuildModuleOfPath fPath
          -> runBuildModuleOfPath state fPath


---------------------------------------------------------------------------------------------------
-- | Load a module interface into the store.
runBuildModuleOfName :: State -> C.ModuleName -> S Bool
runBuildModuleOfName state nModule
 = goHave
 where
        goHave
         -- Always rebuild and relink the main module.
         | nModule == C.ModuleName ["Main"]
         = do   filePath <- queryLocateModule state nModule
                addJob state $ JobBuildModuleOfPath filePath
                return True

         | otherwise
         = do   -- See if we already have the interface.
                -- If the interface is in the store then we assume that it's fresh.
                bHave   <- queryHaveInterface state nModule
                if bHave
                 then   return True
                 else   goEnsure

        goEnsure
         = do   -- See if the interface is already in the store,
                -- or try to load it from a file.
                bInterfaceInStore
                 <- liftIO $ fmap isJust $ C.fetchInterface (stateStore state) nModule

                if bInterfaceInStore
                 then do
                        -- Remember we already have it.
                        addFactHaveInterface state nModule
                        return True

                 else do
                        -- Try to find the source file for the module,
                        -- and add a new job to build the source.
                        filePath <- queryLocateModule state nModule
                        addJob state $ JobBuildModuleOfPath filePath
                        return True


---------------------------------------------------------------------------------------------------
-- | Build a module from a file path.
runBuildModuleOfPath :: State -> FilePath -> S Bool
runBuildModuleOfPath state filePath
 | S.takeExtension filePath == ".ds"
 =      chaseDiscusSourceModuleOfPath state filePath

 | S.takeExtension filePath == ".dct"
 = do   buildCoreDiscusModuleOfPath state filePath
        return True

 | S.takeExtension filePath == ".dcs"
 = do   buildCoreSaltModuleOfPath state filePath
        return True

 | otherwise
 = error $ "ddc-driver.runBuildModuleOfPath: don't know how to build a " ++ show filePath


-- Source Discus ----------------------------------------------------------------------------------
chaseDiscusSourceModuleOfPath :: State -> FilePath -> S Bool
chaseDiscusSourceModuleOfPath state filePath
 = goCheckInterface
 where
        -- Check if we already have the interface of the target module,
        -- or can load it from a file.
        goCheckInterface
         = do   -- TODO: if we already know the deps then check those first.
                (nModule, nsImport)
                 <- queryTasteModuleAtPath state filePath

                bInterfaceInStore
                 <- liftIO $ fmap isJust $ C.fetchInterface (stateStore state) nModule

                if (bInterfaceInStore && (not $ nModule == C.ModuleName ["Main"]))
                 then do
                        addFactHaveInterface state nModule
                        return True

                 else   goCheckDeps nModule nsImport

        -- Check if we already have the interfaces of dependent modules,
        -- or can load them from files.
        goCheckDeps nModule nsImport
         = do   bHaveDeps <- queryHaveInterfaces state nsImport
                if bHaveDeps
                 then   buildDiscusSourceModuleOfPath state filePath nModule

                 else do
                        -- TODO: only add modules we know are not in the interface store.
                        fsPath  <- mapM (queryLocateModule state)
                               $  Set.toList nsImport

                        addJobs state $ map JobBuildModuleOfPath fsPath
                        return False


-- | Build a Discus source module from a file path.
buildDiscusSourceModuleOfPath :: State -> FilePath -> ModuleName -> S Bool
buildDiscusSourceModuleOfPath state filePath nModule
 = do
        liftIO $ putStrLn $ "* Compiling " ++ filePath

        sSource   <- liftIO $ readFile filePath

        -- TODO: just checking the source takes a fraction of the time
        -- of building the .o file. Add job timings.
        mmDiscus
         <- withExceptT ErrorBuild
         $  fmap (C.Reannotate.reannotate (const ()))
         $  G.Discus.sourceLoadText
                (stateConfig state) (stateStore state)
                (SourceFile filePath)
                sSource

        mmDiscus `deepseq` return ()

        -- Store the interface file for later.
        let pathBase    = fromMaybe "." $ configOutputDir $ stateConfig state
        let pathDI      = S.replaceExtension (pathBase S.</> filePath) "di"
        liftIO $ S.createDirectoryIfMissing True (S.takeDirectory pathDI)

        storeInterfaceOfModule state mmDiscus pathDI
        addFactHaveInterface   state nModule

        -- Continue compilation.
        buildDiscusSourceModule state filePath mmDiscus

        return True


-- | Build a Discus source module from the AST representation.
buildDiscusSourceModule :: State -> FilePath -> C.Module () D.Name -> S ()
buildDiscusSourceModule state filePath mmDiscus
 = do
        let config_runtime
                = (configRuntime $ stateConfig state)
                { A.configHookHandleTopLevel
                        = Just (T.pack "Console", T.pack "ddcHookHandleTopLevel") }

        let config_driver
                = (stateConfig state)
                { configRuntime = config_runtime }

        mmSalt
         <- withExceptT ErrorBuild
         $  G.Discus.discusToSalt
                config_driver (SourceFile filePath)
                mmDiscus

        mmSalt `deepseq` return ()

        buildCoreSaltModule state filePath mmSalt


storeInterfaceOfModule state mm pathDI
 = do
        -- Get current time stamp for interface file.
        timeDI  <- liftIO $ S.getCurrentTime

        -- Write out the interface file.
        let int = C.Interface
                { interfaceVersion      = V.version
                , interfaceFilePath     = pathDI
                , interfaceTimeStamp    = timeDI
                , interfaceModuleName   = C.moduleName mm
                , interfaceModule       = C.Reannotate.reannotate (const ()) mm }

        let cEncode
                = C.Encode.Config
                { C.Encode.configTakeRef        = D.Encode.takeName
                , C.Encode.configTakeVarName    = D.Encode.takeVarName
                , C.Encode.configTakeConName    = D.Encode.takeConName }

        liftIO $ C.Encode.storeInterface cEncode pathDI int

        -- Add the new interface to the store.
        liftIO $ C.addInterface (stateStore state) int

        return ()


-- Core Discus ------------------------------------------------------------------------------------
-- | Build a Discus Core Module.
buildCoreDiscusModuleOfPath :: State -> FilePath -> S ()
buildCoreDiscusModuleOfPath state filePath
 = do
        liftIO $ putStrLn $ "* Compiling " ++ filePath

        -- TODO: don't use strings
        sSource   <- liftIO $ readFile filePath

        mmDiscus
         <- withExceptT ErrorBuild
         $  fmap (C.Reannotate.reannotate (const ()))
         $  G.Discus.discusLoadText
                (stateConfig state) (stateStore  state)
                (SourceFile filePath)
                sSource

        mmDiscus `deepseq` return ()

        mmSalt
         <- withExceptT ErrorBuild
         $  G.Discus.discusToSalt
                (stateConfig state) (SourceFile filePath)
                mmDiscus

        mmSalt `deepseq` return ()

        buildCoreSaltModule state filePath mmSalt


-- Core Salt --------------------------------------------------------------------------------------
buildCoreSaltModuleOfPath :: State -> FilePath -> S ()
buildCoreSaltModuleOfPath state filePath
 = do
        liftIO $ putStrLn $ "* Compiling " ++ filePath

        -- TODO: don't use strings
        sSource <- liftIO $ readFile filePath

        mmSalt
         <- withExceptT ErrorBuild
         $  fmap (C.Reannotate.reannotate (const ()))
         $  G.Salt.saltLoadText
                (stateConfig state)
                (SourceFile filePath)
                sSource

        mmSalt `deepseq` return ()

        buildCoreSaltModule state filePath mmSalt


-- | Build a Salt module that has been lowered from Discus code.
buildCoreSaltModule :: State -> FilePath -> C.Module () S.Name -> S ()
buildCoreSaltModule state filePath mmSalt
 = do
        -- Link the file into an executable if this is the Main module.
        let bBuildExe = S.takeBaseName filePath == "Main"

        -- Link in dependent modules.
        let bLinkDeps = bBuildExe && S.takeExtension filePath == ".ds"

        -- Add GC slot stack if we're not compiling a Core Salt file.
        let bSlotify  = S.takeExtension filePath == ".ds"

        -- When building an executable,
        -- get the list of extra .o files we need to link with.
        mfsLink
         <- if bLinkDeps
                then fmap Just $ getLinkObjectsOfModule state (C.moduleName mmSalt)
                else return Nothing

        -- FIXME: this uses up 80% of the compilation time.
        -- What's going wrong? How much is the LLVM compiler?
        withExceptT ErrorBuild
         $ G.Salt.saltCompileViaLlvm
                (stateConfig state)
                (SourceFile filePath)
                mfsLink bSlotify bBuildExe
                mmSalt

        return ()


-- | Get the list of transitive object files we need to link an executable with.
getLinkObjectsOfModule :: State -> ModuleName -> S [FilePath]
getLinkObjectsOfModule state mn
 = do
        -- Get the set of modules transitively imported by this one.
        mmns    <- liftIO $ C.resolveModuleTransitiveDeps (stateStore state) mn
        mns     <- case mmns of
                    Nothing   -> error "getLinkObjectsOfModule: cannot load interface"
                    Just mns' -> return mns'

        -- Find all the original source files for those modules.
        fsDS    <- mapM (queryLocateModule state)
                $  Set.toList $ Set.delete (C.ModuleName ["Main"]) mns

        -- Find the .o files that we've built for each of the modules.
        -- These should all have been built by the time we reach this point,
        -- but they might be located either next to the .ds file or in the
        -- auxilliary output directory.
        let findO mOutputDir fileDS =
             case mOutputDir of
                Just pathOut
                 -> do  let fileO_inAux = S.replaceExtension (pathOut S.</> fileDS) ".o"
                        bHaveInAux <- Status.cachedDoesFileExist (stateStatus state) fileO_inAux
                        if bHaveInAux
                         then return fileO_inAux
                         else findO Nothing fileDS

                Nothing
                 -> do  let fileO_nearDS = S.replaceExtension fileDS ".o"
                        bHaveNearDS <- Status.cachedDoesFileExist (stateStatus state) fileO_nearDS
                        if bHaveNearDS
                         then return fileO_nearDS
                         else error $ "ddc-driver.getLinkObjectsOfModule: missing " ++ fileO_nearDS

        -- Get paths for the corresponding .o files, that need to be next to the source files.
        let mOutputDir = configOutputDir $ stateConfig state
        fsO     <- liftIO $ mapM (findO mOutputDir) fsDS

        return fsO

