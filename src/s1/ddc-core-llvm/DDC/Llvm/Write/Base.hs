
module DDC.Llvm.Write.Base
        ( Config (..), Version
        , configOfVersion
        , configOfHandle
        , module DDC.Data.Write)
where
import DDC.Data.Write
import Data.Maybe
import Data.List
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified System.IO              as S


-- | LLVM Writer configuration.
data Config
        = Config
        { configHandle                  :: S.Handle

        -- The LLVM version that we're targeting.
        , configVersion                 :: Version

        -- Wants meta-data encoded as values.
        , configWantsMetadataAsValue    :: Bool

         -- From LLVM 3.7 we need to give the type of the source pointer
         -- as well as the type of the result of the load. Before this
         -- we only needed to give the type of the source pointer.
        , configWantsLoadReturnTypes    :: Bool }


instance Write Config T.Text where
 write o tx
  = T.hPutStr (configHandle o) tx
 {-# INLINE write #-}


-- | LLVM version descriptor.
type Version = String


-- | Construct a writer config assuming the latest supported LLVM version.
configOfHandle :: S.Handle -> Config
configOfHandle h = configOfVersion h Nothing


-- | Construct a writer config for the given LLVM version.
--
--   Assume LLVM 3.8 as the default version unless told otherwise.
--   This default should only be used during debugging,
--   when compiling real modules the version will be set explicitly
--   by the compilation driver.
--
--   LLVM version 3.8.0 was current as of March 2016.
--
configOfVersion :: S.Handle -> Maybe Version -> Config
configOfVersion h mVersion
 = let  version = fromMaybe "3.8.0" mVersion
   in   Config
        { configHandle  = h
        , configVersion = version

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

