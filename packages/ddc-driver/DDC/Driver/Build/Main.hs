
module DDC.Driver.Build.Main
        ( buildSpec
        , buildComponent
        , buildModule)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Config
import DDC.Build.Spec
import DDC.Build.Interface.Base
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Check         as C
import qualified DDC.Core.Tetra         as Tetra
import qualified DDC.Data.SourcePos     as BP

-- | Annotated interface type.
type InterfaceAA
        = Interface (C.AnTEC BP.SourcePos Tetra.Name) ()


---------------------------------------------------------------------------------------------------
-- | Build all the components defined by a build spec.
buildSpec  
        :: Config               -- ^ Build config.
        -> FilePath             -- ^ File path of build spec.
        -> Spec                 -- ^ Build spec.
        -> ErrorT String IO ()

buildSpec config pathSpec spec
 = do   mapM_   (buildComponent config pathSpec spec) 
                (specComponents spec)


---------------------------------------------------------------------------------------------------
-- | Build a single component of a build spec.
buildComponent 
        :: Config               -- ^ Build config.
        -> FilePath             -- ^ File path of build spec.
        -> Spec                 -- ^ Build spec that mentions the module.
        -> Component            -- ^ Component to build.
        -> ErrorT String IO ()

buildComponent config pathSpec spec component@SpecLibrary{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building " ++ specLibraryName component

        buildLibrary config pathSpec spec component []
         $ specLibraryTetraModules component

        return ()

buildComponent config pathSpec spec component@SpecExecutable{}
 = do   
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building " ++ specExecutableName component

        buildExecutable config pathSpec spec component [] 
                (specExecutableTetraMain         component)
                (specExecutableTetraOther component)

        return ()


---------------------------------------------------------------------------------------------------
-- | Build a library consisting of several modules.
buildLibrary 
        :: Config               -- ^ Build config
        -> FilePath             -- ^ File path of build spec.
        -> Spec                 -- ^ Build spec for the library
        -> Component            -- ^ Component defining the library
        -> [InterfaceAA]        -- ^ Interfaces that we've already loaded.
        -> [String]             -- ^ Names of modules still to build
        -> ErrorT String IO ()

buildLibrary config pathSpec spec component interfaces0 ms0
 = go interfaces0 ms0
 where
        go _interfaces []
         = return ()

        go interfaces (m : more)
         = do   
                moreInterfaces  <- buildModule config pathSpec spec component interfaces m

                let interfaces' = interfaces ++ moreInterfaces
                go  interfaces' more


---------------------------------------------------------------------------------------------------
-- | Build an executable consisting of several modules.
buildExecutable
        :: Config               -- ^ Build config.
        -> FilePath             -- ^ File path of build spec.
        -> Spec                 -- ^ Build spec that mentions the module.
        -> Component            -- ^ Build component that mentions the module.
        -> [InterfaceAA]        -- ^ Interfaces of modules that we've already loaded.
        -> String               -- ^ Name  of main module.
        -> [String]             -- ^ Names of dependency modules
        -> ErrorT String IO ()

buildExecutable config pathSpec spec component interfaces0 _mMain msOther0
 = go interfaces0 msOther0
 where  
        go _interfaces [] 
         = return ()

        go interfaces (m : more)
         = do   
                moreInterfaces  <- buildModule config pathSpec spec component interfaces m

                let interfaces' =  interfaces ++ moreInterfaces
                go  interfaces' more


---------------------------------------------------------------------------------------------------
-- | Build a single module.
buildModule
        :: Config               -- ^ Build config.
        -> FilePath             -- ^ File path of build spec.
        -> Spec                 -- ^ Build spec that mentions the module.
        -> Component            -- ^ Build component that mentions the module.
        -> [InterfaceAA]        -- ^ Interfaces of modules that we've already loaded.
        -> String               -- ^ Module name.
        -> ErrorT String IO [InterfaceAA]

buildModule config pathSpec _spec _component interfaces name
 = do   
        when (configLogBuild config)
         $ liftIO 
         $ do   putStrLn $ "  - compiling " ++ name

        cmdCompile config interfaces
                (resolvePathOfModule pathSpec "ds" name)


-- | Given the path of a .build spec, and a module name, yield the path where
--   the source of the module should be.
resolvePathOfModule 
        :: FilePath             -- ^ Path to build spec.
        -> String               -- ^ Source file extension.
        -> String               -- ^ Module name.
        -> FilePath

resolvePathOfModule pathSpec ext name
 = takeDirectory pathSpec </> go name
 where  go str 
         | elem '.' str
         , (n, '.' : rest)      <- span (/= '.') str
         = n   </> go rest

         | otherwise
         = str <.> ext

