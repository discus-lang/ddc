
module DDC.Core.Check.Context.Oracle
        ( Oracle (..)
        , newOracleOfStore
        , importModules)
where
import DDC.Core.Module.Name
import qualified DDC.Core.Interface.Store       as Store
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
        bs <- mapM (Store.loadInterface (oracleStore oracle)) mns

        when (not $ and bs)
         $ error "ddc-core.Oracle.importModules: store did not load the interfaces we wanted"

        return $ oracle {
                oracleImportedModules
                 = Set.union (Set.fromList mns)
                 $ oracleImportedModules oracle }
