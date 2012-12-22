
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


baseLiteFiles :: Builder -> [FilePath]
baseLiteFiles builder
 = let  _bits    = show $ archPointerWidth $ platformArch $ buildTarget builder
   in   [ "lite" </> "base" </> "Data" </> "Numeric" </> "Bool.dcl"
        , "lite" </> "base" </> "Data" </> "Numeric" </> "Int.dcl"
        , "lite" </> "base" </> "Data" </> "Numeric" </> "Nat.dcl" ]


baseSaltFiles :: Builder -> [FilePath]
baseSaltFiles builder
 = let  bits    = show $ archPointerWidth $ platformArch $ buildTarget builder
   in   [ "salt" </> "runtime"   ++ bits </> "Object.dcs" ]


baseSeaFiles  :: Builder -> [FilePath]
baseSeaFiles _builder
 =      ["sea"  </> "primitive" </> "Primitive.c"]


-- Buid the base libraries and runtime system.
cmdBaseBuild :: Config  -> ErrorT String IO ()
cmdBaseBuild config
 = do   let builder     = configBuilder config
        let target      = buildTarget builder

        -- Ensure the lib dir exists.
        exists   <- liftIO $ doesDirectoryExist $ buildBaseLibDir builder
        when (not exists)
         $ liftIO $ createDirectory $ buildBaseLibDir builder

        -- Build all the .dcl files.
        let srcLiteFiles = map (buildBaseSrcDir builder </>) (baseLiteFiles builder)
        let objLiteFiles = map (flip replaceExtension "o")   srcLiteFiles
        mapM_ (cmdCompile config) srcLiteFiles

        -- Build all the .dcs files.
        let srcSaltFiles = map (buildBaseSrcDir builder </>) (baseSaltFiles builder)
        let objSaltFiles = map (flip replaceExtension "o")   srcSaltFiles
        mapM_ (cmdCompile config) srcSaltFiles

        -- Build all the .c files.
        let srcSeaFiles  = map (buildBaseSrcDir builder </>) (baseSeaFiles builder)
        let objSeaFiles  = map (flip replaceExtension "o")   srcSeaFiles
        liftIO $ zipWithM_ (buildCC builder) srcSeaFiles objSeaFiles

        -- All the .o files
        let objFiles     = objLiteFiles ++ objSaltFiles ++ objSeaFiles

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

