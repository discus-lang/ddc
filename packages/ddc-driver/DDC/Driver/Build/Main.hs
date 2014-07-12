
module DDC.Driver.Build.Main
        ( buildSpec
        , buildComponent
        , buildModule)
where
import DDC.Build.Spec
import DDC.Driver.Config
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class

-- | Build all the components defined by a spec.
buildSpec  
        :: Config       -- ^ Build config.
        -> FilePath     -- ^ File path of build spec.
        -> Spec         -- ^ Build spec.
        -> ErrorT String IO ()

buildSpec config pathSpec spec
 = do   mapM_   (buildComponent config pathSpec spec) 
                (specComponents spec)


-- | Build a single component in a build spec.
buildComponent 
        :: Config       -- ^ Build config.
        -> FilePath     -- ^ File path of build spec.
        -> Spec         -- ^ Build spec that mentions the module.
        -> Component    -- ^ Component to build.
        -> ErrorT String IO ()

buildComponent config pathSpec spec component@SpecLibrary{}
 = do
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "* Building " ++ specLibraryName component

        mapM_ (buildModule config pathSpec spec component)
         $ specLibraryTetraModules component

        return ()


-- | Build a single module.
buildModule
        :: Config       -- ^ Build config.
        -> FilePath     -- ^ File path of build spec.
        -> Spec         -- ^ Build spec that mentions the module.
        -> Component    -- ^ Build component that mentions the module.
        -> String       -- ^ Module name.
        -> ErrorT String IO ()

buildModule config _pathSpec _spec _component name
 = do   
        when (configLogBuild config)
         $ liftIO $ putStrLn $ "  - compiling " ++ name

        return ()
