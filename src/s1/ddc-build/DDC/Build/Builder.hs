
module DDC.Build.Builder
        ( BuilderConfig (..)
        , BuilderHost   (..)
        , Builder       (..)
        , BuilderResult (..)
        , builders

        , determineDefaultBuilder
        , determineDefaultBuilderHost)
where
import DDC.Build.Builder.Base
import DDC.Data.Pretty
import Control.Monad

import DDC.Build.Builder.BuilderX8632Darwin
import DDC.Build.Builder.BuilderX8632Linux
import DDC.Build.Builder.BuilderX8664Darwin
import DDC.Build.Builder.BuilderX8664Linux
import DDC.Build.Builder.BuilderPPC32Linux


-- builders -------------------------------------------------------------------
-- | All supported builders.
--   The host and target platforms are the same.
--
--   Supported builders are:
--      @x86_32-darwin@, @x86_64-darwin@,
--      @x86_32-linux@,  @x86_64-linux@,
--      @ppc32-linux@
--
builders :: BuilderConfig -> BuilderHost -> [Builder]
builders config host
 =      [ builder_X8632_Darwin config host Nothing
        , builder_X8664_Darwin config host Nothing
        , builder_X8632_Linux  config host
        , builder_X8664_Linux  config host
        , builder_PPC32_Linux  config host ]


-- defaultBuilder -------------------------------------------------------------
-- | Determine the default builder based on the 'arch' and 'uname' commands.
--   This assumes that the 'host' and 'target' platforms are the same.
--
--   If we don't recognise the result of 'arch' or 'uname', or don't have
--   a default builder config for this platform then `Nothing`.
--
determineDefaultBuilder :: BuilderConfig -> IO (Maybe Builder)
determineDefaultBuilder config
 = do   mPlatform       <- determineHostPlatform
        mHost           <- determineDefaultBuilderHost

        case (mPlatform, mHost) of
         (Right (Platform ArchX86_32 (OsDarwin mVersion)), Just host)
                -> return $ Just (builder_X8632_Darwin config host mVersion)

         (Right (Platform ArchX86_64 (OsDarwin mVersion)), Just host)
                -> return $ Just (builder_X8664_Darwin config host mVersion)

         (Right (Platform ArchX86_32 OsLinux),  Just host)
                -> return $ Just (builder_X8632_Linux  config host)

         (Right (Platform ArchX86_64 OsLinux),  Just host)
                -> return $ Just (builder_X8664_Linux  config host)

         (Right (Platform ArchPPC_32 OsLinux),  Just host)
                -> return $ Just (builder_PPC32_Linux  config host)

         _      -> return Nothing


-- | Determine the default builder host configuration,
--   this the default set of build tools that we can see in the current path.
determineDefaultBuilderHost :: IO (Maybe BuilderHost)
determineDefaultBuilderHost
 = do
        -- Get the version of the LLVM suite in the current path.
        mLlvmVersion  <- determineHostLlvmVersion Nothing
        mLlvmBinPath  <- determineHostLlvmBinPath Nothing

        case liftM2 (,) mLlvmVersion mLlvmBinPath of
         Left err
          -> error $ renderIndent $ ppr err   -- ******** TODO: propagate

         Right (llvmVersion, llvmBinPath)
          -> return  $ Just $ BuilderHost
                     { builderHostLlvmVersion = llvmVersion
                     , builderHostLlvmBinPath = llvmBinPath
                     }

