
module DDC.Core.Interface.Oracle
        ( Oracle (..)
        , Store.TyConThing (..)
        , Store.kindOfTyConThing
        , newOracleOfStore
        , importModules
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName)
where
import DDC.Core.Interface.Store                 (Store)
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import Data.Set                                 (Set)
import Data.Map                                 (Map)
import Data.IORef
import Control.Monad
import qualified DDC.Core.Interface.Store       as Store
import qualified Data.Set                       as Set
import qualified Data.Map.Strict                as Map


---------------------------------------------------------------------------------------------------
-- | The oracle knows the types and kinds of things in external modules
--   (or can find out). It is responsible to managing import visibility
--   for the module being checked, and can demand load more data via the
--   interface store.
--
--   When type checking several modules the same interface store can be
--   reused, but a new Oracle needs to be create to manage import
--   visibility.
--
--   Each of declaration loaded from the Store are added to an internal
--   oracle cache, and this is checked for successive lookups.
--   Using the oracle cache allows us to reduce the number of reads
--   of the store index, and gives us the list of external declarations
--   used by the module. After type checking the list of cached declarations
--   can be used to close the module so it can be re-checked without needing
--   access to external interfaces.
--
data Oracle n
        = Oracle
        { -- | Store remembers data loaded from module interfaces.
          oracleStore                    :: Store n

          -- | These modules have been imported into the current scope.
        , oracleImportedModules          :: Set ModuleName

          -- | Data types previously loaded via the oracle.
        , oracleCacheDataTypesByTyCon    :: IORef (Map n (DataType n))

          -- | Foreign types previously loaded via the oracle.
        , oracleCacheForeignTypesByTyCon :: IORef (Map n (ImportType n (Kind n)))

          -- | Type synonyms previously loaded via the oracle.
        , oracleCacheTypeSynsByTyCon     :: IORef (Map n (Kind n, Type n))

          -- | Data ctors previously loaded via the oracle.
        , oracleCacheDataCtorsByDaCon    :: IORef (Map n (DataCtor n, DataType n))

          -- | Capabilities previously loaded via the oracle.
        , oracleCacheCapsByName          :: IORef (Map n (ImportCap n (Type n)))

          -- | Values previously loaded via the oracle.
        , oracleCacheValuesByName        :: IORef (Map n (ImportValue n (Type n)))
        }


-- | Construct a new Oracle that wraps the given store.
--   The Oracle does not have any imports defined yet.
newOracleOfStore :: Store n -> IO (Oracle n)
newOracleOfStore store
 = do
        refDataTypesByTyCon     <- newIORef Map.empty
        refForeignTypesByTyCon  <- newIORef Map.empty
        refTypeSynsByTyCon      <- newIORef Map.empty
        refDataCtorsByDaCon     <- newIORef Map.empty
        refCapsByName           <- newIORef Map.empty
        refValuesByName         <- newIORef Map.empty

        return
         $ Oracle
         { oracleStore                    = store
         , oracleImportedModules          = Set.empty
         , oracleCacheDataTypesByTyCon    = refDataTypesByTyCon
         , oracleCacheForeignTypesByTyCon = refForeignTypesByTyCon
         , oracleCacheTypeSynsByTyCon     = refTypeSynsByTyCon
         , oracleCacheDataCtorsByDaCon    = refDataCtorsByDaCon
         , oracleCacheCapsByName          = refCapsByName
         , oracleCacheValuesByName        = refValuesByName }


---------------------------------------------------------------------------------------------------
-- | Import bindings for some modules into the current scope.
--
--   FIXME: force load these if they're not already there.
importModules :: (Ord n, Show n) => Oracle n -> [ModuleName] -> IO (Oracle n)
importModules oracle mns
 = do
        -- Check that the store already contains the interface files we need.
        -- This should have been guaranteed by the compilation driver.
        bs <- mapM (Store.ensureInterface (oracleStore oracle)) mns

        -- FIXME: convert hard errors to internal errors.
        when (not $ and bs)
         $ error "ddc-core.Oracle.importModules: store did not load the interfaces we wanted"

        return $ oracle {
                oracleImportedModules
                 = Set.union (Set.fromList mns)
                 $ oracleImportedModules oracle }


---------------------------------------------------------------------------------------------------
-- | Lookup the name of a data type, type synonym or foreign type in
--   an imported module. If we find it then add it to the oracle cache,
--   before returning it. If not then `Nothing`.
resolveTyConThing
        :: (Ord n, Show n)
        => Oracle n -> n
        -> IO (Either (Store.Error n) (Maybe (Store.TyConThing n)))

resolveTyConThing oracle n
 = goCacheDataType
 where
        -- Check the oracle cache for a data type of the desired name.
        goCacheDataType
         = do   cache   <- readIORef (oracleCacheDataTypesByTyCon oracle)
                case Map.lookup n cache of
                 Just dataType  -> return $ Right $ Just $ Store.TyConThingData n dataType
                 Nothing        -> goCacheSyn

        -- Check the oracle cache for a type synonym of the desired name.
        goCacheSyn
         = do   cache   <- readIORef (oracleCacheTypeSynsByTyCon oracle)
                case Map.lookup n cache of
                 Just (k, t)    -> return $ Right $ Just $ Store.TyConThingSyn n k t
                 Nothing        -> goCacheForeignType

        -- Check the oracle cache for a foreign type of the desired name.
        goCacheForeignType
         = do   cache   <- readIORef (oracleCacheForeignTypesByTyCon oracle)
                case Map.lookup n cache of
                 Just it        -> return $ Right $ Just $ Store.TyConThingForeign n it
                 Nothing        -> goStore

        -- Look for a thing of the desired name in the interface store.
        goStore
         = do   r <- Store.resolveTyConThing
                        (oracleStore oracle)
                        (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                -> return $ Right Nothing
                 Left err       -> return $ Left err
                 Right thing    -> goUpdate thing

        -- Update the oracle cache with a found tycon thing.
        goUpdate thing = case thing of
         Store.TyConThingPrim{}
          -> do return  $ Right $ Just thing

         Store.TyConThingData _ dataType
          -> do modifyIORef' (oracleCacheDataTypesByTyCon oracle)
                 $ \dts  -> Map.insert n dataType dts
                return  $ Right $ Just thing

         Store.TyConThingForeign _ it
          -> do modifyIORef' (oracleCacheForeignTypesByTyCon oracle)
                 $ \its -> Map.insert n it its
                return  $ Right $ Just thing

         Store.TyConThingSyn _ k t
          -> do modifyIORef' (oracleCacheTypeSynsByTyCon oracle)
                        $ \decls -> Map.insert n (k, t) decls
                return  $ Right $ Just thing


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a data constructor,
--   caching the result in the oracle if we find it.
resolveDataCtor
        :: (Ord n, Show n)
        => Oracle n -> n
        -> IO (Either (Store.Error n) (Maybe (DataCtor n)))

resolveDataCtor oracle n
 = goCache
 where
        -- Check the oracle cache for a data constructor of the desired name.
        goCache
         = do   dctorTypes
                 <- readIORef (oracleCacheDataCtorsByDaCon oracle)

                case Map.lookup n dctorTypes of
                 Just (dctor, _dtype) -> return $ Right (Just dctor)
                 Nothing              -> goStore

        -- Look for a data constructor of the desired name in the interface store.
        goStore
         = do   r <- Store.resolveDataCtor
                        (oracleStore oracle) (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                       -> return $ Right Nothing
                 Left err              -> return $ Left err
                 Right (dctor, dtype)  -> goUpdate dctor dtype

        -- Update the oracle cache with a found data constructor.
        goUpdate dctor dtype
         = do
                modifyIORef' (oracleCacheDataCtorsByDaCon oracle)
                 $ \dctortypes -> Map.insert n (dctor, dtype) dctortypes

                modifyIORef' (oracleCacheDataTypesByTyCon oracle)
                 $ \dtypes -> Map.insert (dataCtorTypeName dctor) dtype dtypes

                return $ Right (Just dctor)


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a value constructor,
--   caching the result in the oracle if we find it.
resolveValueName
        :: (Ord n, Show n)
        => Oracle n -> n
        -> IO (Either (Store.Error n) (Maybe (ImportValue n (Type n))))

resolveValueName oracle n
 = goCache
 where
        -- Check the oracle cache for a value of the desired name.
        goCache
         = do   cache <- readIORef (oracleCacheValuesByName oracle)
                case Map.lookup n cache of
                 Just iv        -> return $ Right (Just iv)
                 Nothing        -> goStore

        -- Look for a value of the desired name in the interface store.
        goStore
         = do   r <- Store.resolveValueName
                        (oracleStore oracle) (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                -> return $ Right Nothing
                 Left err       -> return $ Left err
                 Right iv       -> goUpdate iv

        -- Update the oracle cache with the type of a found value.
        goUpdate iv
         = do   modifyIORef' (oracleCacheValuesByName oracle)
                 $ \ivs -> Map.insert n iv ivs
                return  $ Right (Just iv)

