
module DDC.Core.Check.Context.Oracle
        ( Oracle (..)
        , newOracleOfStore)
where
import DDC.Core.Module.Name
import DDC.Core.Interface.Store


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
        , oracleImportedModules :: [ModuleName]
        }


-- | Construct a new Oracle that wraps the given store.
--   The Oracle does not have any imports defined yet.
newOracleOfStore :: Store n -> Oracle n
newOracleOfStore store
        = Oracle
        { oracleStore           = store
        , oracleImportedModules = [] }


