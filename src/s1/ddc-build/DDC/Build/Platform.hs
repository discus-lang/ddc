
module DDC.Build.Platform
        ( -- * Data types
          Platform      (..)
        , staticFileExtensionOfPlatform
        , sharedFileExtensionOfPlatform

        , Arch          (..)
        , archPointerWidth

        , Os            (..)

          -- * Host platform determination
        , Error (..)
        , determineHostPlatform
        , determineHostArch
        , determineHostOs
        , determineHostLlvmVersion
        , determineHostLlvmBinPath )
where
import DDC.Build.Platform.Base
import DDC.Build.Platform.Determine
import DDC.Build.Platform.Error
