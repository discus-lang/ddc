
module DDC.Driver.Build.Main
        ( buildSpec
        , buildComponent
        , buildModule)
where
import DDC.Driver.Command.Compile
import DDC.Driver.Config
import DDC.Build.Spec
import System.FilePath
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

buildModule config pathSpec _spec _component name
 = do   
        when (configLogBuild config)
         $ liftIO 
         $ do   putStrLn $ "  - compiling " ++ name

        cmdCompile config 
                (resolvePathOfModule pathSpec "dst" name)

        return ()


-- | Given the path of a .build spec, and a module name, yield the path where
--   the source of the module should be.
resolvePathOfModule 
        :: FilePath             -- Path to build spec.
        -> String               -- Source file extension.
        -> String               -- Module name.
        -> FilePath

resolvePathOfModule pathSpec ext name
 = takeDirectory pathSpec </> go name
 where  go str 
         | elem '.' str
         , (n, '.' : rest)      <- span (/= '.') str
         = n   </> go rest

         | otherwise
         = str <.> ext

