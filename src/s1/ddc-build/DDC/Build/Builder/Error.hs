
module DDC.Build.Builder.Error
        (Error (..))
where
import DDC.Build.Builder.Base
import DDC.Data.Pretty
import qualified DDC.Build.Platform       as Platform


-------------------------------------------------------------------------------
-- | Things that can go wrong when determining the default builder.
--
--   We want to be very specific why we can't determine the build platform
--   as this is a complete blocker to people getting started with the project.
--
data Error
        -- | Cannot determine host platform.
        = ErrorPlatformDetermination
        { errorPlatformError    :: Platform.Error }

        -- | We recognize the build host but don't have a builder spec for it.
        | ErrorBuilderUnsupported
        { errorPlatform         :: Platform.Platform
        , errorHost             :: BuilderHost }
        deriving Show


-------------------------------------------------------------------------------
instance Pretty Error where
 ppr (ErrorPlatformDetermination err)
  = vcat
  [ text "Cannot determine the host platform."
  , indent 1 $ ppr err ]

 ppr (ErrorBuilderUnsupported platform host)
  = vcat
  [ text "Build platform is not currently supported."
  , ppr platform
  , text "Host LLVM version: " % string (builderHostLlvmVersion host)
  , text "Host LLVM path:    " % string (builderHostLlvmBinPath host) ]

