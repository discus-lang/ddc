
module DDC.Core.Check.Context.Oracle
        ( Oracle (..)
        , Store.TyConThing (..)
        , Store.kindOfTyConThing
        , newOracleOfStore
        , importModules
        , resolveTyConThing
        , resolveDataCtor
        , resolveValueName)
where
import DDC.Core.Check.State
import DDC.Core.Check.Error
import DDC.Type.DataDef
import DDC.Core.Module
import DDC.Type.Exp
import DDC.Core.Interface.Store                 (Store)
import Data.Set                                 (Set)
import Data.Map                                 (Map)
import Data.IORef
import Control.Monad
import qualified DDC.Core.Interface.Store       as Store
import qualified DDC.Core.Interface.Resolve     as Store
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
          oracleStore                   :: Store n

          -- | These modules have been imported into the current scope.
        , oracleImportedModules         :: Set ModuleName

          -- | Data types previously loaded via the oracle.
        , oracleCacheDataTypesByTyCon   :: IORef (Map n (DataType n))

          -- | Data ctors previously loaded via the oracle.
        , oracleCacheDataCtorsByDaCon   :: IORef (Map n (DataCtor n))

          -- | Type synonyms previously loaded via the oracle.
        , oracleCacheTypeDefsByTyCon    :: IORef (Map n (Kind n, Type n))

          -- | Capabilities previously loaded via the oracle.
        , oracleCacheCapsByName         :: IORef (Map n (ImportCap n (Type n)))

          -- | Values previously loaded via the oracle.
        , oracleCacheValuesByName       :: IORef (Map n (ImportValue n (Type n)))
        }


-- | Construct a new Oracle that wraps the given store.
--   The Oracle does not have any imports defined yet.
newOracleOfStore :: Store n -> IO (Oracle n)
newOracleOfStore store
 = do
        refDataTypesByTyCon     <- newIORef Map.empty
        refDataCtorsByDaCon     <- newIORef Map.empty
        refTypeDefsByTyCon      <- newIORef Map.empty
        refCapsByName           <- newIORef Map.empty
        refValuesByName         <- newIORef Map.empty

        return
         $ Oracle
         { oracleStore                   = store
         , oracleImportedModules         = Set.empty
         , oracleCacheDataTypesByTyCon   = refDataTypesByTyCon
         , oracleCacheDataCtorsByDaCon   = refDataCtorsByDaCon
         , oracleCacheTypeDefsByTyCon    = refTypeDefsByTyCon
         , oracleCacheCapsByName         = refCapsByName
         , oracleCacheValuesByName       = refValuesByName }


---------------------------------------------------------------------------------------------------
-- | Import bindings for some modules into the current scope.
--   The store already needs to have the
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
-- TODO: convert the rest of the errors.
-- TODO: add tests for these errors.
checkOfResolveError :: (Ord n, Show n) => n -> Store.Error n -> Error a n
checkOfResolveError n err
 = case err of
        Store.ErrorNotFound _   -> ErrorType $ ErrorTypeUndefinedTypeCtor (UName n)
        _                       -> error $ "some error " ++ show err


---------------------------------------------------------------------------------------------------
-- FIXME: add oracle cache for things that we've found.
--   We want to cache visible things relative to the module being checked,
--   rather than hitting the Store indexes each time.
--   We can also then use the oracle cache to close the module after type checking,
--   as it is guaranteed to contain all used declarations.
resolveTyConThing
        :: (Ord n, Show n)
        => Oracle n -> n -> CheckM a n (Maybe (Store.TyConThing n))

resolveTyConThing oracle n
 = goCacheType
 where  goCacheType
         = do   cache   <- liftIO $ readIORef (oracleCacheDataTypesByTyCon oracle)
                case Map.lookup n cache of
                 Just dataType  -> return $ Just $ Store.TyConThingData n dataType
                 Nothing        -> goCacheSyn

        goCacheSyn
         = do   cache   <- liftIO $ readIORef (oracleCacheTypeDefsByTyCon oracle)
                case Map.lookup n cache of
                 Just (k, t)    -> return $ Just $ Store.TyConThingSyn n k t
                 Nothing        -> goStore

        goStore
         = do   r       <- liftIO $ Store.resolveTyConThing
                                (oracleStore oracle)
                                (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                -> return Nothing
                 Left err       -> throw $ checkOfResolveError n err
                 Right thing    -> goUpdate thing

        goUpdate thing@(Store.TyConThingData _ dataType)
         = do   liftIO  $ modifyIORef' (oracleCacheDataTypesByTyCon oracle)
                        $ \dts  -> Map.insert n dataType dts
                return  $ Just thing

        goUpdate thing@(Store.TyConThingSyn _ k t)
         = do   liftIO  $ modifyIORef' (oracleCacheTypeDefsByTyCon oracle)
                        $ \decls -> Map.insert n (k, t) decls
                return  $ Just thing


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a data constructor,
--   caching the result in the oracle if we find it.
resolveDataCtor
        :: (Ord n, Show n)
        => Oracle n -> n -> CheckM a n (Maybe (DataCtor n))

resolveDataCtor oracle n
 = goCache
 where  goCache
         = do   cache <- liftIO $ readIORef (oracleCacheDataCtorsByDaCon oracle)
                case Map.lookup n cache of
                 Just ctor      -> return $ Just ctor
                 Nothing        -> goStore

        goStore
         = do   r     <- liftIO $ Store.resolveDataCtor
                                (oracleStore oracle) (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                -> return Nothing
                 Left err       -> throw $ checkOfResolveError n err
                 Right ctor     -> goUpdate ctor

        goUpdate ctor
         = do   liftIO  $ modifyIORef' (oracleCacheDataCtorsByDaCon oracle)
                        $ \ctors -> Map.insert n ctor ctors
                return  $ Just ctor


---------------------------------------------------------------------------------------------------
-- | Resolve the name of a value constructor,
--   caching the result in the oracle if we find it.
resolveValueName
        :: (Ord n, Show n)
        => Oracle n -> n -> CheckM a n (Maybe (Store.ImportValue n (Type n)))

resolveValueName oracle n
 = goCache
 where  goCache
         = do   cache   <- liftIO $ readIORef (oracleCacheValuesByName oracle)
                case Map.lookup n cache of
                 Just iv        -> return $ Just iv
                 Nothing        -> goStore

        goStore
         = do   r       <- liftIO $ Store.resolveValueName
                                (oracleStore oracle) (oracleImportedModules oracle) n
                case r of
                 Left Store.ErrorNotFound{}
                                -> return Nothing
                 Left err       -> throw $ checkOfResolveError n err
                 Right iv       -> goUpdate iv

        goUpdate iv
         = do   liftIO  $ modifyIORef' (oracleCacheValuesByName oracle)
                        $ \ivs -> Map.insert n iv ivs
                return  $ Just iv

