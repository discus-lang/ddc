
module DDC.Driver.Build.Run where
import DDC.Driver.Build.Query
import DDC.Driver.Build.State
import DDC.Driver.Interface.Source
import DDC.Driver.Config
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Interface                     as Interface
import qualified Data.Set                               as Set

import qualified DDC.Driver.Stage.Tetra                 as Stage.Discus
import qualified DDC.Driver.Stage.Salt                  as Stage.Salt

import qualified DDC.Core.Salt                          as Salt
import qualified DDC.Core.Discus                        as Discus
import qualified DDC.Version                            as Version
import qualified DDC.Core.Transform.Reannotate          as C.Reannotate
import qualified DDC.Core.Codec.Shimmer.Encode          as C.Encode
import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as D.Encode
import qualified Data.Time.Clock                        as Time
-- import Control.Monad

import qualified DDC.Core.Salt.Runtime                  as A
import Control.DeepSeq

import qualified System.FilePath                        as S
import qualified Data.Text                              as T
-- import qualified System.Directory                    as S



-- | Try to run a job, returning True if it made progress.
runJob :: State -> Job -> S Bool
runJob state job
 = case job of
         JobBuildModuleOfName nModule
          -> runBuildModuleOfName state nModule

         JobBuildModuleOfPath fPath
          -> runBuildModuleOfPath state fPath


-- | Load a module interface into the store.
runBuildModuleOfName :: State -> C.ModuleName -> S Bool
runBuildModuleOfName state nModule
 = do
        -- See if our build state knows we already have the interface.
        bHave   <- queryHaveInterface state nModule
        if bHave
         then return True
         else do
                -- See if the interface is already in the store,
                -- or try to load it from a file.
                bInterfaceInStore
                 <- liftIO $ Interface.ensureInterface (stateStore state) nModule
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
 = do   -- TODO: if we already know the deps then check those first.
        (_nModule, nsImport)
         <- queryTasteModuleAtPath state filePath

        bHaveDeps
         <- queryHaveInterfaces state nsImport

        if bHaveDeps
         then   buildModuleOfPath state filePath
         else do
                -- TODO: only add modules we know are not in the interface store.
                addJobs state
                 $ map JobBuildModuleOfName
                 $ Set.toList nsImport

                return False


---------------------------------------------------------------------------------------------------
-- | Build a source module, deciding what do do based on the file extension.
buildModuleOfPath :: State -> FilePath -> S Bool
buildModuleOfPath state filePath
 = do
        liftIO $ putStrLn $ "* Compiling " ++ filePath

        buildDiscusSourceModuleOfPath state filePath
        return True


---------------------------------------------------------------------------------------------------
-- | Build a Discus source module from a file path.
buildDiscusSourceModuleOfPath :: State -> FilePath -> S ()
buildDiscusSourceModuleOfPath state filePath
 = do
        sSource   <- liftIO $ readFile filePath


        -- TODO: just checking the source take a fraction of the time
        -- of building the .o file. Add job timings.
        mmDiscus
         <- withExceptT ErrorBuild
         $  fmap (C.Reannotate.reannotate (const ()))
         $  Stage.Discus.sourceLoadText
                (stateConfig state)
                (stateStore state)
                (SourceFile filePath)
                sSource

        mmDiscus `deepseq` return ()

        -- Store the interface file for later.
        let pathDI = S.replaceExtension filePath "di"
        storeInterfaceOfModule state mmDiscus pathDI

        -- Continue compilation.
        buildDiscusSourceModule state filePath mmDiscus


---------------------------------------------------------------------------------------------------
-- | Build a Discus source module from the AST representation.
buildDiscusSourceModule :: State -> FilePath -> C.Module () Discus.Name -> S ()
buildDiscusSourceModule state filePath mmDiscus
 = do
        let config
                = stateConfig state

        let config_runtime
                = (configRuntime config)
                { A.configHookHandleTopLevel
                        = Just (T.pack "Console", T.pack "ddcHookHandleTopLevel") }

        let config_driver
                = config
                { configRuntime = config_runtime }

        mmSalt
         <- withExceptT ErrorBuild
         $  Stage.Discus.discusToSalt
                config_driver (SourceFile filePath)
                [] -- mnsTrans
                mmDiscus

        buildDiscusSaltModule state filePath mmSalt


---------------------------------------------------------------------------------------------------
-- | Build a Salt module from the AST representation.
buildDiscusSaltModule :: State -> FilePath -> C.Module () Salt.Name -> S ()
buildDiscusSaltModule state filePath mmSalt
 = do
        -- Link the file into an executable if this is the Main module.
        let bBuildExe
                = S.takeBaseName filePath == "Main"

        -- Add GC slot stack if we're not compiling a Core Salt file.
        let bSlotify
                = not $ S.takeExtension filePath == ".dcs"

        -- TODO: this uses up 80% of the compilation time.
        withExceptT ErrorBuild
         $ Stage.Salt.saltCompileViaLlvm
                (stateConfig state)
                (SourceFile filePath)
                Nothing -- [] -- otherObjs
                bSlotify
                bBuildExe
                mmSalt

        return ()


---------------------------------------------------------------------------------------------------
storeInterfaceOfModule state mm pathDI
 = do
        -- Get current time stamp for interface file.
        timeDI  <- liftIO $ Time.getCurrentTime

        -- Write out the interface file.
        let int = Interface.Interface
                { interfaceVersion      = Version.version
                , interfaceFilePath     = pathDI
                , interfaceTimeStamp    = timeDI
                , interfaceModuleName   = C.moduleName mm
                , interfaceModule       = C.Reannotate.reannotate (const ()) mm }

        let cEncode
                = C.Encode.Config
                { C.Encode.configTakeRef        = D.Encode.takeName
                , C.Encode.configTakeVarName    = D.Encode.takeVarName
                , C.Encode.configTakeConName    = D.Encode.takeConName }

        liftIO  $ C.Encode.storeInterface cEncode pathDI int

        -- Add the new interface to the store.
        liftIO $ Interface.addInterface (stateStore state) int

        return ()


