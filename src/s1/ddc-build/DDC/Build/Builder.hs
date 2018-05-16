
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
import DDC.Build.Builder.Error
import qualified DDC.Build.Platform     as Platform

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
determineDefaultBuilder :: BuilderConfig -> IO (Either Error Builder)
determineDefaultBuilder config
 = goDeterminePlatform
 where
        goDeterminePlatform
         = Platform.determineHostPlatform
         >>= \case
                Left err   -> return $ Left $ ErrorPlatformDetermination err
                Right platform -> goDetermineHost platform

        goDetermineHost platform
         = determineDefaultBuilderHost
         >>= \case
                Left err   -> return $ Left err
                Right host -> goChooseBuilder platform host

        goChooseBuilder platform host
         = case platform of
                Platform ArchX86_32 (OsDarwin mVersion)
                  -> return $ Right $ builder_X8632_Darwin config host mVersion

                Platform ArchX86_64 (OsDarwin mVersion)
                  -> return $ Right $ builder_X8664_Darwin config host mVersion

                Platform ArchX86_32 OsLinux
                  -> return $ Right $ builder_X8632_Linux  config host

                Platform ArchX86_64 OsLinux
                  -> return $ Right $ builder_X8664_Linux  config host

                Platform ArchPPC_32 OsLinux
                  -> return $ Right $ builder_PPC32_Linux  config host

                _ -> return $ Left  $ ErrorBuilderUnsupported platform host


-- | Determine the default builder host configuration,
--   this the default set of build tools that we can see in the current path.
determineDefaultBuilderHost :: IO (Either Error BuilderHost)
determineDefaultBuilderHost
 = goGetLlvmVersion
 where
        goGetLlvmVersion
         = Platform.determineHostLlvmVersion Nothing
         >>= \case
                Left err        -> return $ Left $ ErrorPlatformDetermination err
                Right version   -> goGetLlvmPath version

        goGetLlvmPath version
         = Platform.determineHostLlvmBinPath Nothing
         >>= \case
                Left err        -> return $ Left $ ErrorPlatformDetermination err
                Right path
                 -> return $ Right $ BuilderHost
                        { builderHostLlvmVersion = version
                        , builderHostLlvmBinPath = path }

