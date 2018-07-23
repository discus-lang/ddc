
module DDC.Driver.Command.Build
        (cmdBuild)
where
import DDC.Driver.Config
import DDC.Driver.Command.Compile
import DDC.Driver.Interface.Status              (Status)
import DDC.Core.Interface.Store                 (Store)
import DDC.Build.Spec
import DDC.Data.Pretty
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified System.FilePath                as FilePath
import qualified DDC.Build.Spec.Parser          as Spec
import qualified DDC.Build.Builder              as Builder
import qualified DDC.Driver.Interface.Status    as Status
import qualified DDC.Driver.Interface.Locate    as Driver
import qualified DDC.Core.Discus                as D
import qualified DDC.Core.Module                as C
import qualified DDC.Data.Pretty                as P
import qualified Data.List                      as List


---------------------------------------------------------------------------------------------------
-- Perform a build following a build specification.
cmdBuild :: Config -> Store D.Name -> FilePath -> ExceptT String IO ()
cmdBuild config store filePath

 -- Build from a build spec file
 | ".build"      <- FilePath.takeExtension filePath
 = do
        -- Search for modules in the base library as well as the same directory
        -- the build file is in.
        let config'
                = config
                { configModuleBaseDirectories
                        =  List.nub
                        $  configModuleBaseDirectories config
                        ++ [ FilePath.takeDirectory filePath
                           , Builder.buildBaseSrcDir (configBuilder config)
                                FilePath.</> "base" ]
                }

        -- Parse the spec file.
        status   <- liftIO $ Status.newStatus
        str      <- liftIO $ readFile filePath
        case Spec.parseBuildSpec filePath str of
         Left err       -> throwE $ renderIndent $ ppr err
         Right spec     -> buildSpec config' status store spec


 -- If we were told to build a source file then just compile it instead.
 -- This is probably the least surprising behaviour.
 | ".ds"        <- FilePath.takeExtension filePath
 = do   cmdCompileRecursive config False store [filePath]
        return ()

 -- Don't know how to build from this file.
 | otherwise
 = let  ext     = FilePath.takeExtension  filePath
   in   throwE $ "Cannot build from '" ++ ext ++ "' files."


---------------------------------------------------------------------------------------------------
-- | Build all the components defined by a build spec.
buildSpec
        :: Config -> Status -> Store D.Name -> Spec
        -> ExceptT String IO ()
buildSpec config status store spec
 = do   mapM_   (buildComponent config status store)
                (specComponents spec)


---------------------------------------------------------------------------------------------------
-- | Build a single component of a build spec.
buildComponent
        :: Config -> Status -> Store D.Name -> Component
        -> ExceptT String IO ()
buildComponent config status store component@SpecLibrary{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building library " ++ specLibraryName component

        buildLibrary config status store
         $ specLibraryTetraModules component

        return ()

buildComponent config status store component@SpecExecutable{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building executable " ++ specExecutableName component

        buildExecutable config status store
                (specExecutableTetraMain  component)
                (specExecutableTetraOther component)

        return ()


---------------------------------------------------------------------------------------------------
-- | Build a library consisting of several modules.
buildLibrary
        :: Config -> Status -> Store D.Name -> [C.ModuleName]
        -> ExceptT String IO ()
buildLibrary config status store ms0
 = go ms0
 where
        go []
         = return ()

        go (m : more)
         = do   buildModule config status store m
                go more


---------------------------------------------------------------------------------------------------
-- | Build an executable consisting of several modules.
buildExecutable
        :: Config               -- ^ Build config.
        -> Status               -- ^ File system status cache.
        -> Store D.Name         -- ^ Interface store.
        -> C.ModuleName         -- ^ Name  of main module.
        -> [C.ModuleName]       -- ^ Names of dependency modules
        -> ExceptT String IO ()

buildExecutable config status store mMain msOther0
 = go msOther0
 where
        go []
         = do   let dirs = configModuleBaseDirectories config

                path
                 <- liftIO (Driver.locateModuleFromPaths status dirs mMain "source" "ds")
                 >>= \case
                        Left err -> throwE $ P.renderIndent $ P.ppr err
                        Right ii -> return ii

                _       <- cmdCompileRecursive config True store [path]
                return ()

        go (m : more)
         = do   buildModule config status store m
                go more


---------------------------------------------------------------------------------------------------
-- | Build a single module.
buildModule
        :: Config               -- ^ Build config.
        -> Status               -- ^ File system status cache.
        -> Store D.Name         -- ^ Interface store.
        -> C.ModuleName         -- ^ Module name.
        -> ExceptT String IO ()

buildModule config status store name
 = do   let dirs = configModuleBaseDirectories config

        path
         <- liftIO (Driver.locateModuleFromPaths status dirs name "source" "ds")
         >>= \case
                Left err -> throwE $ P.renderIndent $ P.ppr err
                Right ii -> return ii

        _       <- cmdCompileRecursive config False store [path]
        return ()
