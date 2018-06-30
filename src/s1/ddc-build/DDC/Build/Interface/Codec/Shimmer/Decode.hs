
module DDC.Build.Interface.Codec.Shimmer.Decode
        (decodeInterface)
where
import DDC.Core.Module                                  as C
import DDC.Type.Env
import DDC.Build.Interface.Error
import DDC.Build.Interface.Base
import Data.Time.Clock
import qualified SMR.Core.Codec                         as SMR
import qualified DDC.Core.Transform.SpreadX             as SpreadX
import qualified DDC.Core.Codec.Shimmer.Decode          as Core.Decode
import qualified Data.ByteString                        as BS


decodeInterface
        :: (Show n, Ord n)
        => Core.Decode.Config n
        -> Env n
        -> Env n
        -> FilePath             -- ^ Path of interace file, for error messages.
        -> UTCTime              -- ^ Timestamp of interface file.
        -> BS.ByteString        -- ^ Interface file contents.
        -> Either Error (Interface n)

decodeInterface config kenv tenv filePath timeStamp bs
 | Just mm <- Core.Decode.takeModuleDecls config
           $  SMR.unpackFileDecls bs
 = let
        mm_spread       = SpreadX.spreadX kenv tenv mm

   in   Right $ Interface
        { interfaceFilePath     = filePath
        , interfaceTimeStamp    = timeStamp
        , interfaceVersion      = "version"
        , interfaceModuleName   = C.moduleName mm_spread
        , interfaceModule       = Just $ mm_spread }

 | otherwise
 = Left ErrorParseEnd

-- import qualified DDC.Core.Fragment                      as C
-- import qualified DDC.Core.Discus                        as D
-- import qualified DDC.Core.Discus.Codec.Shimmer.Decode   as Discus.Decode
--        kenv            = C.profilePrimKinds D.profile
--        tenv            = C.profilePrimTypes D.profile
-- where  configDiscus
--         = Core.Decode.Config
--         { Core.Decode.configTakeRef = Discus.Decode.takeName }