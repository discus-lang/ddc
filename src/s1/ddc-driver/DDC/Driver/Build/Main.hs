
module DDC.Driver.Build.Main
        ( buildSpec
        , buildComponent
        , buildModule)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Build.Locate
import DDC.Driver.Config
import DDC.Build.Spec
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import DDC.Core.Interface.Store         (Store)
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Discus        as D


---------------------------------------------------------------------------------------------------
-- | Build all the components defined by a build spec.
buildSpec
        :: Config               -- ^ Build config.
        -> Store D.Name         -- ^ Interface store.
        -> Spec                 -- ^ Build spec.
        -> ExceptT String IO ()

buildSpec config store spec
 = do   mapM_   (buildComponent config store)
                (specComponents spec)


---------------------------------------------------------------------------------------------------
-- | Build a single component of a build spec.
buildComponent
        :: Config               -- ^ Build config.
        -> Store D.Name         -- ^ Interface store.
        -> Component            -- ^ Component to build.
        -> ExceptT String IO ()

buildComponent config store component@SpecLibrary{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building library " ++ specLibraryName component

        buildLibrary config store
         $ specLibraryTetraModules component

        return ()

buildComponent config store component@SpecExecutable{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building executable " ++ specExecutableName component

        buildExecutable config store
                (specExecutableTetraMain  component)
                (specExecutableTetraOther component)

        return ()


---------------------------------------------------------------------------------------------------
-- | Build a library consisting of several modules.
buildLibrary
        :: Config               -- ^ Build config
        -> Store D.Name         -- ^ Interface store.
        -> [C.ModuleName]       -- ^ Names of modules still to build
        -> ExceptT String IO ()

buildLibrary config store ms0
 = go ms0
 where
        go []
         = return ()

        go (m : more)
         = do   buildModule config store m
                go more


---------------------------------------------------------------------------------------------------
-- | Build an executable consisting of several modules.
buildExecutable
        :: Config               -- ^ Build config.
        -> Store D.Name         -- ^ Interface store.
        -> C.ModuleName         -- ^ Name  of main module.
        -> [C.ModuleName]       -- ^ Names of dependency modules
        -> ExceptT String IO ()

buildExecutable config store mMain msOther0
 = go msOther0
 where
        go []
         = do   let dirs = configModuleBaseDirectories config
                path     <- locateModuleFromPaths dirs mMain "ds"
                _        <- cmdCompile config True [] store path
                return ()

        go (m : more)
         = do   buildModule config store m
                go more


---------------------------------------------------------------------------------------------------
-- | Build a single module.
buildModule
        :: Config               -- ^ Build config.
        -> Store D.Name         -- ^ Interface store.
        -> C.ModuleName         -- ^ Module name.
        -> ExceptT String IO ()

buildModule config store name
 = do   let dirs = configModuleBaseDirectories config
        path    <- locateModuleFromPaths dirs name "ds"
        _       <- cmdCompile config False [] store path
        return ()


