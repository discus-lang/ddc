
module DDC.Build.Interface.Codec.Shimmer.Encode
        ( storeInterface
        , encodeInterfaceDecls)
where
import DDC.Build.Interface.Base
import Data.Text                                        (Text)

import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as Discus.Encode
import qualified DDC.Core.Codec.Shimmer.Encode          as Core.Encode

import qualified SMR.Core.Codec                         as Shimmer
import qualified SMR.Core.Exp                           as Shimmer
import qualified SMR.Prim.Name                          as Shimmer

import qualified Foreign.Marshal.Alloc                  as Foreign
import qualified System.IO                              as System


-- | Store an interface at the given file path.
storeInterface :: FilePath -> Interface ta sa -> IO ()
storeInterface pathDst int
 = do
        let dsDecls     = encodeInterfaceDecls int
        let len         = Shimmer.sizeOfFileDecls dsDecls
        Foreign.allocaBytes len $ \pBuf
         -> do  _ <- Shimmer.pokeFileDecls dsDecls pBuf
                h <- System.openBinaryFile pathDst System.WriteMode
                System.hPutBuf h pBuf len
                System.hClose h
                return ()


-- | Convert an interface to a list of Shimmer declarations.
encodeInterfaceDecls
        :: Interface ta sa
        -> [Shimmer.Decl Text Shimmer.Prim]
encodeInterfaceDecls int
 | Just m       <- interfaceDiscusModule int
 = Core.Encode.takeModuleDecls
        (Core.Encode.Config
        { Core.Encode.configTakeRef     = Discus.Encode.takeName
        , Core.Encode.configTakeVarName = Discus.Encode.takeVarName
        , Core.Encode.configTakeConName = Discus.Encode.takeConName })
        m

 | otherwise = []



