
module DDC.Driver.Build.Main
        ( buildSpec
        , buildComponent
        , buildModule)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Build.Locate
import DDC.Driver.Config
import DDC.Build.Spec
import DDC.Build.Interface.Base
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Check         as C
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Tetra         as Tetra
import qualified DDC.Data.SourcePos     as BP

-- | Annotated interface type.
type InterfaceAA
        = Interface (C.AnTEC BP.SourcePos Tetra.Name) ()


---------------------------------------------------------------------------------------------------
-- | Build all the components defined by a build spec.
buildSpec  
        :: Config               -- ^ Build config.
        -> Spec                 -- ^ Build spec.
        -> ErrorT String IO ()

buildSpec config spec
 = do   mapM_   (buildComponent config) 
                (specComponents spec)


---------------------------------------------------------------------------------------------------
-- | Build a single component of a build spec.
buildComponent 
        :: Config               -- ^ Build config.
        -> Component            -- ^ Component to build.
        -> ErrorT String IO ()

buildComponent config component@SpecLibrary{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building library " ++ specLibraryName component

        buildLibrary config []
         $ specLibraryTetraModules component

        return ()

buildComponent config component@SpecExecutable{}
 = do   
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building executable " ++ specExecutableName component

        buildExecutable config [] 
                (specExecutableTetraMain  component)
                (specExecutableTetraOther component)

        return ()


---------------------------------------------------------------------------------------------------
-- | Build a library consisting of several modules.
buildLibrary 
        :: Config               -- ^ Build config
        -> [InterfaceAA]        -- ^ Interfaces that we've already loaded.
        -> [C.ModuleName]       -- ^ Names of modules still to build
        -> ErrorT String IO ()

buildLibrary config interfaces0 ms0
 = go interfaces0 ms0
 where
        go _interfaces []
         = return ()

        go interfaces (m : more)
         = do   interfaces'     <- buildModule config interfaces m
                go  interfaces' more


---------------------------------------------------------------------------------------------------
-- | Build an executable consisting of several modules.
buildExecutable
        :: Config               -- ^ Build config.
        -> [InterfaceAA]        -- ^ Interfaces of modules that we've already loaded.
        -> C.ModuleName         -- ^ Name  of main module.
        -> [C.ModuleName]       -- ^ Names of dependency modules
        -> ErrorT String IO [InterfaceAA]

buildExecutable config interfaces0 mMain msOther0
 = go interfaces0 msOther0
 where  
        go interfaces [] 
         = do   
                let dirs        = configModuleBaseDirectories config
                path            <- locateModuleFromPaths dirs mMain "ds"
                cmdCompile config True interfaces path

        go interfaces (m : more)
         = do   interfaces'     <- buildModule config interfaces m
                go  interfaces' more


---------------------------------------------------------------------------------------------------
-- | Build a single module.
buildModule
        :: Config               -- ^ Build config.
        -> [InterfaceAA]        -- ^ Interfaces of modules that we've already loaded.
        -> C.ModuleName         -- ^ Module name.
        -> ErrorT String IO [InterfaceAA]

buildModule config interfaces name
 = do   
        let dirs = configModuleBaseDirectories config
        path     <- locateModuleFromPaths dirs name "ds"

        cmdCompile config False interfaces path


