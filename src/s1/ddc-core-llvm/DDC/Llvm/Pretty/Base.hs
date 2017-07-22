
module DDC.Llvm.Pretty.Base
        ( Config(..)
        , Version
        , defaultConfig
        , configOfVersion
        , versionWantsMetadataAsValue
        , versionWantsLoadReturnTypes)
where
import Data.List
import Data.Maybe


-------------------------------------------------------------------------------
-- | LLVM pretty printer configuration.
data Config
        = Config
        { configLlvmVersion             :: Version
        , configWantsMetadataAsValue    :: Bool
        , configWantsLoadReturnTypes    :: Bool }
        deriving Show


-- | LLVM version descriptor.
type Version
        = String


-- | Default config that can be used for debugging.
defaultConfig :: Config
defaultConfig =  configOfVersion Nothing


-- | Produce a default pretty printer config for the given version.
--
--   Assume LLVM 3.8 as the default version unless told otherwise.
--   This default should only be used during debugging,
--   when compiling real modules the version will be set explicitly
--   by the compilation driver.
--
--   LLVM version 3.8.0 was current as of March 2016.
--
configOfVersion :: Maybe Version -> Config
configOfVersion mVersion
 = let  version = fromMaybe "3.8.0" mVersion
   in   Config
        { configLlvmVersion             = version

        , configWantsMetadataAsValue
                = fromMaybe False  $ versionWantsMetadataAsValue version

        , configWantsLoadReturnTypes
                = fromMaybe True   $ versionWantsLoadReturnTypes version }


-- | LLVM versions before 3.6.1 encoded meta data as a value,
--   while after that it is typeless.
versionWantsMetadataAsValue :: Version -> Maybe Bool
versionWantsMetadataAsValue v
        | isPrefixOf "3.1." v   = Just True
        | isPrefixOf "3.2." v   = Just True
        | isPrefixOf "3.3." v   = Just True
        | isPrefixOf "3.4." v   = Just True
        | isPrefixOf "3.5." v   = Just True
        | isPrefixOf "3.6"  v   = Just False
        | isPrefixOf "3.7"  v   = Just False
        | isPrefixOf "3.8"  v   = Just False
        | isPrefixOf "3.9"  v   = Just False
        | otherwise             = Nothing


-- | LLVM versions before 3.7.0 did not use a result type on load
--   and getelembyptr operations, while after they did.
versionWantsLoadReturnTypes :: Version -> Maybe Bool
versionWantsLoadReturnTypes v
        | isPrefixOf "3.1." v   = Just False
        | isPrefixOf "3.2." v   = Just False
        | isPrefixOf "3.3." v   = Just False
        | isPrefixOf "3.4." v   = Just False
        | isPrefixOf "3.5." v   = Just False
        | isPrefixOf "3.6." v   = Just False
        | isPrefixOf "3.7." v   = Just True
        | isPrefixOf "3.8." v   = Just True
        | isPrefixOf "3.9." v   = Just True
        | otherwise             = Nothing


