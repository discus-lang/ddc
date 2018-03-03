
module DDC.Build.Interface.Codec.Shimmer.Decode
        (decodeInterface)
where
import DDC.Build.Interface.Error
import DDC.Build.Interface.Base
import Data.Time.Clock
import DDC.Core.Module                                  as C
import qualified SMR.Core.Codec                         as SMR
import qualified DDC.Core.Discus.Codec.Shimmer.Decode   as Discus.Decode
import qualified DDC.Core.Codec.Shimmer.Decode          as Core.Decode
import qualified Data.ByteString                        as BS


decodeInterface
        :: FilePath             -- ^ Path of interace file, for error messages.
        -> UTCTime              -- ^ Timestamp of interface file.
        -> BS.ByteString        -- ^ Interface file contents.
        -> Either Error InterfaceAA

decodeInterface filePath timeStamp bs
 | Just modDiscus
        <- Core.Decode.takeModuleDecls configDiscus
        $  SMR.unpackFileDecls bs
 = Right $ Interface
        { interfaceFilePath     = filePath
        , interfaceTimeStamp    = timeStamp
        , interfaceVersion      = "version"
        , interfaceModuleName   = C.moduleName modDiscus
        , interfaceDiscusModule = Just $ modDiscus }

 | otherwise
 = Left ErrorParseEnd

 where  configDiscus
         = Core.Decode.Config
         { Core.Decode.configTakeRef = Discus.Decode.takeName }