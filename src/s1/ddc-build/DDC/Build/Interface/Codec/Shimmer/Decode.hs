
module DDC.Build.Interface.Codec.Shimmer.Decode
        (decodeInterfaceDecls)
where
import DDC.Build.Interface.Base
import DDC.Core.Module                                  as C

import qualified SMR.Core.Exp                           as Shimmer
import qualified SMR.Prim.Name                          as Shimmer

import qualified DDC.Core.Discus.Codec.Shimmer.Decode   as Discus.Decode
import qualified DDC.Core.Codec.Shimmer.Decode          as Core.Decode

import Data.Text                                        (Text)
import Data.Time.Clock


decodeInterfaceDecls
        :: FilePath
        -> UTCTime
        -> [Shimmer.Decl Text Shimmer.Prim]
        -> Maybe (Interface () sa)

decodeInterfaceDecls filePath timeStamp ds
 | Just modDiscus <- Core.Decode.takeModuleDecls configDiscus ds
 = Just $ Interface
        { interfaceFilePath     = filePath
        , interfaceTimeStamp    = timeStamp
        , interfaceVersion      = "version"
        , interfaceModuleName   = C.moduleName modDiscus
        , interfaceDiscusModule = Just $ modDiscus
        , interfaceSaltModule   = Nothing }


 | otherwise
 = Nothing

 where  configDiscus
         = Core.Decode.Config
         { Core.Decode.configTakeRef = Discus.Decode.takeName }