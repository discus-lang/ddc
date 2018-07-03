
module DDC.Core.Check.Context.Oracle
        ( Oracle (..)
        , Store.TyConThing (..)
        , Store.kindOfTyConThing
        , newOracleOfStore
        , importModules
        , resolveTyConThing)
where
import DDC.Core.Check.State
import DDC.Core.Module.Name
import DDC.Core.Check.Error
import DDC.Type.Exp
import qualified DDC.Core.Interface.Store       as Store
import qualified DDC.Core.Interface.Resolve     as Store
import qualified Data.Set                       as Set
import DDC.Core.Interface.Store                 (Store)
import Data.Set                                 (Set)
import Control.Monad


-- | The oracle knows the types and kinds of things in external modules
--   (or can find out). It is responsible to managing import visibility
--   for the module being checked, and can demand load more data via the
--   interface store.
--
--   When type checking multiple modules the same interface store can be
--   reused, which caches the loaded data, but a new Oracle needs to be
--   created to manage import visibility.
--
data Oracle n
        = Oracle
        { -- | Store remembers data loaded from module interfaces.
          oracleStore           :: Store n

          -- | These modules have been imported into the current scope.
        , oracleImportedModules :: Set ModuleName
        }


-- | Construct a new Oracle that wraps the given store.
--   The Oracle does not have any imports defined yet.
newOracleOfStore :: Store n -> Oracle n
newOracleOfStore store
        = Oracle
        { oracleStore           = store
        , oracleImportedModules = Set.empty }


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


-- FIXME: add oracle cache for things that we've found.
--   We want to cache visible things relative to the module being checked,
--   rather than hitting the Store indexes each time.
--   We can also then use the oracle cache to close the module after type checking,
--   as it is guaranteed to contain all used declarations.
resolveTyConThing
        :: forall n a. (Ord n, Show n)
        => Oracle n -> n -> CheckM a n (Maybe (Store.TyConThing n))

resolveTyConThing oracle n
 = goStore
 where
        goStore
         = (liftIO $ Store.resolveTyConThing
                        (oracleStore oracle)
                        (oracleImportedModules oracle) n)
         >>= \case
                Left Store.ErrorNotFound{} -> return Nothing
                Left err    -> throw  $ checkOfResolveError n err
                Right thing -> return $ Just thing



-- TODO: convert the rest of the errors.
-- TODO: add tests for these errors.
checkOfResolveError :: (Ord n, Show n) => n -> Store.Error n -> Error a n
checkOfResolveError n err
 = case err of
        Store.ErrorNotFound _   -> ErrorType $ ErrorTypeUndefinedTypeCtor (UName n)
        _                       -> error $ "some error " ++ show err

