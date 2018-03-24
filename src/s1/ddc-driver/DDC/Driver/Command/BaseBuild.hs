
module DDC.Driver.Command.BaseBuild
        (cmdBaseBuild)
where
import DDC.Driver.Stage
import DDC.Build.Platform
import DDC.Build.Builder
import DDC.Driver.Command.Compile
import System.FilePath
import System.Directory
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad
import DDC.Build.Interface.Store        (Store)


baseSaltFiles :: Builder -> [FilePath]
baseSaltFiles builder
 = let  bits    = show $ archPointerWidth $ platformArch $ buildTarget builder
        runtime = "ddc-runtime" </> "salt" </> "runtime"
   in   [ runtime         </> "Alloc.dcs"
        , runtime         </> "Collect.dcs"
        , runtime         </> "Hook.dcs"
        , runtime         </> "Init.dcs"
        , runtime ++ bits </> "debug"     </> "Check.dcs"
        , runtime ++ bits </> "debug"     </> "Trace.dcs"
        , runtime ++ bits </> "primitive" </> "Array.dcs"
        , runtime ++ bits </> "primitive" </> "Env.dcs"
        , runtime ++ bits </> "primitive" </> "Numeric.dcs"
        , runtime ++ bits </> "primitive" </> "Ref.dcs"
        , runtime ++ bits </> "primitive" </> "Text.dcs"
        , runtime ++ bits </> "primitive" </> "Vector.dcs"
        , runtime ++ bits </> "Apply.dcs"
        , runtime ++ bits </> "Object.dcs" ]


baseSeaFiles  :: Builder -> [FilePath]
baseSeaFiles _builder
 =      ["ddc-runtime" </> "sea"  </> "Primitive.c"]


-- Buid the base libraries and runtime system.
cmdBaseBuild :: Config  -> Store -> ExceptT String IO ()
cmdBaseBuild config store
 = do   let builder     = configBuilder config
        let target      = buildTarget builder

        -- Ensure the lib dir exists.
        let dirBuild    = buildBaseLibDir builder </> "ddc-runtime" </> "build"
        exists   <- liftIO $ doesDirectoryExist dirBuild
        when (not exists)
         $ liftIO $ createDirectory $ dirBuild

        -- Build all the .dcs files.
        let config'      = config { configInferTypes = True }
        let srcSaltFiles = map (buildBaseSrcDir builder </>) (baseSaltFiles builder)
        let objSaltFiles = map (flip replaceExtension "o")   srcSaltFiles
        mapM_ (cmdCompile config' False [] store) srcSaltFiles

        -- Build all the .c files.
        let srcSeaFiles  = map (buildBaseSrcDir builder </>) (baseSeaFiles builder)
        let objSeaFiles  = map (flip replaceExtension "o")   srcSeaFiles
        liftIO $ zipWithM_ (buildCC builder) srcSeaFiles objSeaFiles

        -- All the .o files
        let objFiles     = objSaltFiles ++ objSeaFiles

        -- Link the .o files into a static library.
        let staticRuntime
                =    dirBuild
                </> "libddc-runtime." ++ staticFileExtensionOfPlatform target

        liftIO $ buildLdLibStatic builder objFiles staticRuntime


        -- Link the .o files into a shared library.
        let sharedRuntime
                =    dirBuild
                </> "libddc-runtime." ++ sharedFileExtensionOfPlatform target

        liftIO $ buildLdLibShared builder objFiles sharedRuntime

        return ()

