
module DDC.Driver.Interface.Status
        ( Status (..)
        , newStatus
        , purgeCacheForFile
        , cachedDoesFileExist
        , cachedModificationTimeIfExists)
where
import Data.IORef
import Data.Map                         (Map)
import qualified System.Directory       as System
import qualified Data.Time.Clock        as System
import qualified Data.Map.Strict        as Map


-------------------------------------------------------------------------------
-- | Cache of file system status.
--
--   During a single run of the compiler we assume that the files do not change
--   as we are running. This allows us to cache filesystem information such as
--   whether a file exists or not, and the modification time. We specifically
--   want to avoid asking the OS file system what the modification time is for
--   all transitively imported interface files, for all modules being compiled.
--
data Status
        = Status
        { -- | Map of file names to whether they exist or not.
          statusFileExists       :: IORef (Map FilePath Bool)

          -- | Map of file names to their modification times.
        , statusModificationTime :: IORef (Map FilePath System.UTCTime) }


-------------------------------------------------------------------------------
-- | Construct a new empty status cache.
newStatus :: IO Status
newStatus
 = do   refFileExists           <- newIORef Map.empty
        refModificationTime     <- newIORef Map.empty
        return  $ Status
                { statusFileExists       = refFileExists
                , statusModificationTime = refModificationTime }


-- | Purge information about the given file.
--   This needs to be done when we modify a file ourselves.
purgeCacheForFile :: Status -> FilePath -> IO ()
purgeCacheForFile status filePath
 = do
        modifyIORef' (statusFileExists status)
         $ \ss -> Map.delete filePath ss

        modifyIORef' (statusModificationTime status)
         $ \ss -> Map.delete filePath ss


-- | Check if a file exists or not.
cachedDoesFileExist :: Status -> FilePath -> IO Bool
cachedDoesFileExist status filePath
 = do   exists <- readIORef (statusFileExists status)
        case Map.lookup filePath exists of
         Just b -> return b
         Nothing
          -> do b <- System.doesFileExist filePath
                modifyIORef' (statusFileExists status) $ \mp
                 -> Map.insert filePath b mp
                return b


-- | Check if a file exists, and if it does get its modification time.
cachedModificationTimeIfExists
        :: Status -> FilePath -> IO (Maybe System.UTCTime)
cachedModificationTimeIfExists status filePath
 = do   bExists <- cachedDoesFileExist status filePath
        if not bExists
         then return Nothing
         else do
                modTime <- readIORef (statusModificationTime status)
                case Map.lookup filePath modTime of
                 Just t -> return $ Just t
                 Nothing
                  -> do t <- System.getModificationTime filePath
                        modifyIORef' (statusModificationTime status) $ \mp
                         -> Map.insert filePath t mp
                        return $ Just t

