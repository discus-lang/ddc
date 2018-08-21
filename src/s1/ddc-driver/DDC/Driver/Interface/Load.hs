
module DDC.Driver.Interface.Load
        ( loadOrDeleteFreshInterface
        , loadInterface
        , ErrorLoad(..))
where
import System.Directory
import Data.IORef
import DDC.Data.Pretty
import DDC.Core.Module                          as C
import DDC.Core.Discus                          as D
import DDC.Core.Interface.Store                 (Store)
import DDC.Driver.Interface.Status              (Status)
import qualified DDC.Driver.Interface.Status    as Status
import qualified DDC.Driver.Interface.Locate    as Locate
import qualified DDC.Core.Interface.Store       as Store
import qualified DDC.Core.Codec.Shimmer.Decode  as Decode
import qualified SMR.Core.Exp                   as SMR
import qualified SMR.Prim.Name                  as SMR
import qualified Data.ByteString                as BS
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified System.FilePath                as System
import qualified System.Directory               as System


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Load a fresh interface from a named file.
--   If the interface file exists but is not fresh then `Nothing`.
--
--   When loading it we inspect the set of transitive dependencies and
--   check that all the dependent interfaces are also fresh. This is done using
--   the file modification times in the file system cache so that we don't end
--   up making a quadratic number of file modification time requests to the
--   operating system.
--
loadOrDeleteFreshInterface
        :: (SMR.Exp Text SMR.Prim -> Maybe D.Name)
                                -- ^ Function to load a name.
        -> [FilePath]           -- ^ Base paths to look for imported modules.
        -> Status               -- ^ File status cache.
        -> Store D.Name         -- ^ Interface store for trans. module deps.
        -> FilePath             -- ^ Path to load file from.
        -> ModuleName           -- ^ Name of the module at the given path.
        -> IO (Either ErrorLoad (Maybe (Store.Interface D.Name)))

loadOrDeleteFreshInterface takeRef pathsBase status store pathDI nModule
 = goStore
 where
        -- Check if we already have the interface in the store.
        -- If we do then we assume it's fresh.
        goStore
         = do   iis <- readIORef $ Store.storeInterfaces store
                case Map.lookup nModule iis of
                 Just ii -> return $ Right $ Just ii
                 Nothing -> goCheckFreshLocal

        -- Check if the interface is locally fresh.
        goCheckFreshLocal
         = do   bFreshLocal <- interfaceIsLocallyFresh status pathDI
                if not bFreshLocal
                 then return $ Right Nothing
                 else goLoadInterface

        -- The interface file is locally fresh, so load it to get the
        -- transitive dependencies. If all the transitive deps are fresh
        -- then we can safely add it to the store to avoid loading it again,
        -- otherwise we delete it to avoid trying to reload a known stale
        -- interface.
        goLoadInterface
         = do   -- Try to load the root interface
                -- Don't add it to the store yet as we don't know
                -- if it's transitivly fresh.
                eInt      <- loadInterface takeRef pathDI
                case eInt of
                 Left err -> return $ Left err
                 Right ii -> goLocateTransDeps ii

        goLocateTransDeps ii
         = do
                -- Locate all the transitively imported modules.
                ePathsDeps
                 <- fmap sequence
                 $  mapM (\mn -> Locate.locateModuleFromPaths
                                   status pathsBase mn "interface" ".di")
                 $  Set.toList $ moduleTransitiveDeps $ Store.interfaceModule ii

                case ePathsDeps of
                 Left _          -> return $ Right Nothing
                 Right pathsDeps -> goCheckTransDeps ii pathsDeps

        goCheckTransDeps ii pathsDeps
         = do
                bsFreshDeps <- mapM (interfaceIsLocallyFresh status) pathsDeps

                if and bsFreshDeps
                 -- Ok, this module and all its deps are fresh.
                 -- We can safely store this one and use it again later.
                 then do
                        Store.addInterface store ii
                        return $ Right $ Just ii

                 -- We know that one of the transitively needed interfaces
                 -- is not fresh, so delete the current one to avoid trying
                 -- to load it again.
                 else do
                        System.removeFile pathDI
                        Status.purgeCacheForFile status pathDI
                        return $ Right Nothing


-- | Check if an interface file is locally fresh,
--   meaning there is both a source and object file in the same dir
--   and the object and interface are older than the source.
interfaceIsLocallyFresh :: Status -> FilePath -> IO Bool
interfaceIsLocallyFresh status pathDI
 = do
        let pathDS  = System.replaceExtension pathDI ".ds"
        let pathO   = System.replaceExtension pathDI ".o"

        mTimeDS <- Status.cachedModificationTimeIfExists status pathDS
        mTimeDI <- Status.cachedModificationTimeIfExists status pathDI
        mTimeO  <- Status.cachedModificationTimeIfExists status pathO

        -- Interface exists and is fresher than source.
        -- See Note: Timestamp accuracy during rebuild.
        let bFreshDI
                | Just timeDS   <- mTimeDS
                , Just timeDI   <- mTimeDI
                , timeDS <= timeDI = True
                | otherwise        = False

        -- Object exists and is fresher than source.
        -- See Note: Timestamp accuracy during rebuild.
        let bFreshO
                | Just timeDS   <- mTimeDS
                , Just timeO    <- mTimeO
                , timeDS <= timeO  = True
                | otherwise        = False

        return $ bFreshDI && bFreshO


---------------------------------------------------------------------------------------------------
-- | Load a new interface from a named file.
loadInterface
        :: (Show n, Ord n)
        => (SMR.Exp Text SMR.Prim -> Maybe n)
                                -- ^ Function to load a name.
        -> FilePath             -- ^ Path to load file from.
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


---------------------------------------------------------------------------------------------------
-- [Note: Timestamp acccuracy during rebuild]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- There's an ugly system issue where if the underlying file system does
-- not support file time stamps with sub-second accuracy, then the
-- timestamps of the interface files we compile in this run will have more
-- accuracy than the ones we load from the file system.
--
-- The problem with inaccurate timestamps is that if we compiled two
-- dependent modules within the same second, then both will have the
-- same time-stamp and none is fresher than the other.
--
-- Due to this we allow the time stamp of dependent interface files to
-- be equal so that they will not be rebuilt in this situation.
--
-- We assume that if any process legitimately changes a dependent
-- object file then this will be done at least a second after we first
-- created it.
--
