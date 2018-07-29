
module DDC.Driver.Interface.Load
        ( loadFreshInterface
        , loadInterface
        , ErrorLoad(..))
where
import DDC.Data.Pretty
import System.Directory
import DDC.Core.Discus                          as D
import DDC.Core.Interface.Store                 (Store)
import DDC.Driver.Interface.Status              (Status)
--import qualified DDC.Driver.Interface.Status    as Status
import qualified DDC.Core.Interface.Store       as Store
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
-- | Load a fresh interface from a named file.
--
--   If the interface file exists but is not fresh then `Nothing`.
--
--   When loading it we inspect the set of transitive dependencies and
--   check that all the dependent interfaces are also fresh. This is done
--   using the file modification times in the file system cache so that we
--   don't end up making a quadratic number of file modification time requests
--   to the operating system.
--
loadFreshInterface
        :: (SMR.Exp Text SMR.Prim -> Maybe D.Name)
                                -- ^ Function to load a name.
        -> Status               -- ^ File status cache.
        -> Store D.Name         -- ^ Interface store for trans. module deps.
        -> FilePath             -- ^ Path to load file from.
        -> IO (Either ErrorLoad (Store.Interface D.Name))

loadFreshInterface takeRef _status _store filePath
 = do   putStrLn $ "* loadFreshInterface " ++ show filePath

        -- TODO: check if the current interface is fresh.

        -- Load the root interface
        ii      <- loadInterface takeRef filePath

        return ii


-------------------------------------------------------------------------------
-- | Load a new interface from a named file.
loadInterface
        :: (Show n, Ord n)
        => (SMR.Exp Text SMR.Prim -> Maybe n)   -- ^ Function to load a name.
        -> FilePath                             -- ^ Path to load file from.
        -> IO (Either ErrorLoad (Store.Interface n))

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

