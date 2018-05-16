
module DDC.Build.Platform
        ( -- * Data types
          Platform      (..)
        , staticFileExtensionOfPlatform
        , sharedFileExtensionOfPlatform

        , Arch          (..)
        , archPointerWidth

        , Os            (..)

          -- * Host platform determination
        , determineHostPlatform
        , determineHostArch
        , determineHostOs
        , determineHostLlvmVersion
        , determineHostLlvmBinPath )
where
import DDC.Build.Platform.Base
import DDC.Build.Platform.Determine
