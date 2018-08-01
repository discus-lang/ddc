
module DDC.Build.Platform
        ( -- * Platform
          Platform      (..)
        , staticFileExtensionOfPlatform
        , sharedFileExtensionOfPlatform

          -- * Architecture
        , Arch          (..)
        , archPointerWidth

          -- * Os
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
