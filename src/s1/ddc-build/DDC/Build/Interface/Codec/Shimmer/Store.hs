{-# LANGUAGE OverloadedStrings #-}
module DDC.Build.Interface.Codec.Shimmer.Store
        (storeInterface)
where
import DDC.Build.Interface.Base
import Data.Text                                        (Text)

import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as Discus.Encode
import qualified DDC.Core.Codec.Shimmer.Encode          as Core

import qualified SMR.Codec.Size                         as Shimmer
import qualified SMR.Codec.Poke                         as Shimmer
import qualified SMR.Core.Exp                           as Shimmer
import qualified SMR.Prim.Op.Base                       as Shimmer

import qualified Foreign.Marshal.Alloc                  as Foreign
import qualified System.IO                              as System


-- | Store an interface at the given file path.
storeInterface :: FilePath -> Interface ta sa -> IO ()
storeInterface pathDst int
 = do
        let decls       = takeInterfaceDecls int

        let len         = Shimmer.sizeOfFile decls
        Foreign.allocaBytes len $ \pBuf
         -> do  _ <- Shimmer.pokeFileDecls decls pBuf
                h <- System.openBinaryFile pathDst System.WriteMode
                System.hPutBuf h pBuf len
                System.hClose h
                return ()


takeInterfaceDecls :: Interface ta sa -> [SDecl]
takeInterfaceDecls int
 | Just m       <- interfaceDiscusModule int
 = Core.takeModuleDecls
        (Core.Config
        { Core.configTakeRef     = Discus.Encode.takeName
        , Core.configTakeVarName = Discus.Encode.takeVarName
        , Core.configTakeConName = Discus.Encode.takeConName })
        m

 | otherwise
 = []

 -- Base -------------------------------------------------------------------------------------------
type SDecl = Shimmer.Decl Text Shimmer.Prim


