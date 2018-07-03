
module DDC.Build.Interface.Load
        ( ErrorLoad(..)
        , loadInterface)
where
import DDC.Core.Interface
import DDC.Data.Pretty
import System.Directory
import qualified DDC.Core.Codec.Shimmer.Decode  as Decode
import qualified SMR.Core.Exp                   as SMR
import qualified SMR.Prim.Name                  as SMR
import qualified Data.ByteString                as BS


-------------------------------------------------------------------------------
-- | Things that can go wrong when loading an interface file.
data ErrorLoad
        = ErrorLoadBadMagic
        { errorLoadFilePath     :: FilePath }

        | ErrorLoadDecodeFailed
        { errorLoadFilePath     :: FilePath }
        deriving Show


instance Pretty ErrorLoad where
 ppr (ErrorLoadBadMagic path)
  = vcat
  [ text "Bad magic when loading interface file:"
  , indent 4 $ string path
  , text "  this file is either corrupted, or not really an interface file." ]

 ppr (ErrorLoadDecodeFailed path)
  = vcat
  [ text "Decode of interface file failed."
  , indent 4 $ string path ]


-------------------------------------------------------------------------------
-- | Load a new interface from a named file.
loadInterface
        :: (Show n, Ord n)
        => (SMR.Exp Text SMR.Prim -> Maybe n)   -- ^ Function to load a name.
        -> FilePath                             -- ^ Path to load file from.
        -> IO (Either ErrorLoad (Interface n))

loadInterface takeRef filePath
 = do
        let config = Decode.Config takeRef

        -- The interface representation has the file modification time attached.
        timeStamp  <- getModificationTime filePath

        -- Read the file source.
        bs <- BS.readFile filePath

        -- Check for the Shimmer format magic numbers at the start of the file,
        -- to avoid accidently trying to load a renamed source file or some
        -- older interface version.
        let magicSMR    = BS.pack [0x53, 0x4d, 0x52, 0x31]

        if BS.isPrefixOf magicSMR (BS.take 4 bs)
         -- Binary interface file in Shimmer format.
         then do
                let iint = Decode.decodeInterface config filePath timeStamp bs
                case iint of
                 Just i  -> return $ Right i
                 Nothing -> return $ Left $ ErrorLoadDecodeFailed filePath

         -- Some other file that isn't an interface.
         else do
                return $ Left $ ErrorLoadBadMagic filePath
