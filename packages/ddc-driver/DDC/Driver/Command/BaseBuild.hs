
module DDC.Driver.Command.BaseBuild
        (cmdBaseBuild)
where
import DDC.Driver.Stage
import DDC.Build.Platform
import DDC.Build.Builder
import DDC.Driver.Command.Compile
import System.FilePath
import System.Directory
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Control.Monad


baseSeaFiles  :: Builder -> [FilePath]
baseSeaFiles _builder
 =      ["sea"  </> "primitive" </> "Primitive.c"]

baseSaltFiles :: Builder -> [FilePath]
baseSaltFiles builder
 = let  bits    = show $ archPointerWidth $ platformArch $ buildTarget builder
   in   [ "salt" </> "runtime"   ++ bits </> "Object.dcs"
        , "salt" </> "primitive" ++ bits </> "Int.dcs" ]


-- Buid the base libraries and runtime system.
cmdBaseBuild :: Config  -> ErrorT String IO ()
cmdBaseBuild config
 = do   let builder     = configBuilder config
        let target      = buildTarget builder

        -- Ensure the lib dir exists.
        exists   <- liftIO $ doesDirectoryExist $ buildBaseLibDir builder
        when (not exists)
         $ liftIO $ createDirectory $ buildBaseLibDir builder

        -- Build all the .c files.
        let srcSeaFiles  = map (buildBaseSrcDir builder </>) (baseSeaFiles builder)
        let objSeaFiles  = map (flip replaceExtension "o")   srcSeaFiles
        liftIO $ zipWithM_ (buildCC builder) srcSeaFiles objSeaFiles

        -- Build all the .dcs files.
        let srcSaltFiles = map (buildBaseSrcDir builder </>) (baseSaltFiles builder)
        let objSaltFiles = map (flip replaceExtension "o")   srcSaltFiles
        mapM_ (cmdCompile config) srcSaltFiles

        -- All the .o files
        let objFiles     = objSeaFiles ++ objSaltFiles


        -- Link the .o files into a static library.
        let staticRuntime 
                = buildBaseLibDir builder
                </> "libddc-runtime." ++ staticFileExtensionOfPlatform target

        liftIO $ buildLdLibStatic builder objFiles staticRuntime


        -- Link the .o files into a shared library.
        let sharedRuntime 
                = buildBaseLibDir builder
                </> "libddc-runtime." ++ sharedFileExtensionOfPlatform target

        liftIO $ buildLdLibShared builder objFiles sharedRuntime

        return ()
