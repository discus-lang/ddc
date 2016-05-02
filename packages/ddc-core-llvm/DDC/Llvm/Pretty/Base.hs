
module DDC.Llvm.Pretty.Base
        ( Config(..)
        , configDefault
        , Version
        , versionWantsMetadataAsValue)
where
import Data.List


-- | LLVM pretty printer configuration.
data Config
        = Config
        { configLlvmVersion     :: Version }


-- | Default config.
configDefault :: Config
configDefault
        = Config
        { -- | Assume LLVM 3.8 as the default version unless told otherwise.
          --   This default should only be used during debugging,
          --   when compiling real modules the version will be set explicitly
          --   by the compilation driver.
          --
          --   LLVM version 3.8.0 was current as of March 2016.
          configLlvmVersion     = "LLVM version 3.8.0" }


-- | LLVM version descriptor.
type Version    
        = String


-- | In LLVM versions before 3.6.1 encoded meta data as a value,
--   while after that it is typeless.
versionWantsMetadataAsValue :: Version -> Maybe Bool
versionWantsMetadataAsValue version
 = case words version of
        ["LLVM", "version",  num]
         | isPrefixOf "3.1." num  -> Just True
         | isPrefixOf "3.2." num  -> Just True
         | isPrefixOf "3.3." num  -> Just True
         | isPrefixOf "3.4." num  -> Just True
         | isPrefixOf "3.5." num  -> Just True
         | isPrefixOf "3."   num  -> Just False

        _                         -> Nothing
