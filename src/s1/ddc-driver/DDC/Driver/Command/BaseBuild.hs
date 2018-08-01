
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
import DDC.Core.Interface.Store         (Store)
import qualified DDC.Core.Discus        as D


-- | Salt source files that form the runtime system and are build with -basebuild.
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
        , runtime ++ bits </> "primitive" </> "Reflect.dcs"
        , runtime ++ bits </> "primitive" </> "Ref.dcs"
        , runtime ++ bits </> "primitive" </> "Text.dcs"
        , runtime ++ bits </> "primitive" </> "Vector.dcs"
        , runtime ++ bits </> "Apply.dcs"
        , runtime ++ bits </> "Object.dcs"
        , runtime ++ bits </> "Info.dcs" ]


-- | Sea source files that form the runtime system and are built with -basebuild.
baseSeaFiles  :: Builder -> [FilePath]
baseSeaFiles _builder
 =      [ "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Console.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Errno.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Error.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Exception.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "File.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Parse.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Show.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "primitive" </> "Text.c"
        , "ddc-runtime" </> "sea" </> "runtime" </> "Collect.c" ]


-- Buid the base libraries and runtime system.
cmdBaseBuild :: Config  -> Store D.Name -> ExceptT String IO ()
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
        cmdCompileRecursive config' False store srcSaltFiles

        -- Build all the .c files.
        let srcSeaFiles  = map (buildBaseSrcDir builder </>) (baseSeaFiles builder)
        let objSeaFiles  = map (flip replaceExtension "o")   srcSeaFiles
        liftIO $ zipWithM_ (buildCC builder) srcSeaFiles objSeaFiles

        -- All the .o files
        let objFiles     = objSaltFiles ++ objSeaFiles

        -- Link the .o files into a static library.
        let pathStaticRuntime
                =    dirBuild
                </> "libddc-runtime." ++ staticFileExtensionOfPlatform target

        liftIO $ buildLdLibStatic builder objFiles pathStaticRuntime

        -- Link the .o files into a shared library.
        let pathSharedRuntime
                =    dirBuild
                </> "libddc-runtime." ++ sharedFileExtensionOfPlatform target

        liftIO $ buildLdLibShared builder objFiles pathSharedRuntime

        return ()

