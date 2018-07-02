
module DDC.Core.Interface.Base
        ( Store         (..)
        , Meta          (..)
        , Interface     (..))
where
import DDC.Core.Module
import DDC.Type.DataDef
import DDC.Type.Exp
import Data.Time.Clock
import Data.IORef
import Data.Map         (Map)
import Data.Set         (Set)


---------------------------------------------------------------------------------------------------
-- | Abstract API to a collection of module interfaces.
--
--   The store is a global data structure that is not specific to one module.
--   It contains information about loaded interfaces, but does not specify
--   whether that information should be visible to a particular client module.
--
--   We do not store client specific visibility here because we want to reuse
--   the loaded data when compiling multiple separate client modules.
--
data Store n
        = Store
        { -- | Metadata for module interfaces currently represented in the store.
          storeMeta             :: IORef (Map ModuleName Meta)

          -- Name Indexes ---------------------
          -- These maps are used when resolving qualified names to determine
          -- which module a name is defined in. They just say where a thing of that
          -- name is defined, not whether it should be visible to the client module.

          -- | Map of type constructor names to modules that define one of that name.
          --   The type name can be of a foreign type, data type, or type synonym.
          --   A client will need to check the other fields of the store to determine which.
        , storeTypeCtorNames        :: IORef (Map n (Set ModuleName))

          -- | Map of data constructor names to modules that define one of that name.
        , storeDataCtorNames        :: IORef (Map n (Set ModuleName))

          -- | Map of capability names to modules that define one of that name.
        , storeCapNames             :: IORef (Map n (Set ModuleName))

          -- | Map of value names to modules that define one of that name.
        , storeValueNames           :: IORef (Map n (Set ModuleName))

          -- Module Exports --------------------
          -- | Map of type names that a module exports.
        , storeExportTypesOfModule  :: IORef (Map ModuleName (Set n))

          -- | Map of value names that a module exports.
        , storeExportValuesOfModule :: IORef (Map ModuleName (Set n))

          -- Module Data -----------------------
          -- These maps contain information about top level declarations of a module
          -- in unpacked form. The outer ModuleName is the name of the original module
          -- that a thing was defined in, not the module that one might have imported
          -- it via. Transitively imported things are not necessearally visible to a
          -- client module, so the visibility needs to be determined separately.

          -- | Map of data type names to their declarations.
        , storeDataDefsByTyCon      :: IORef (Map ModuleName (Map n (DataDef n)))

          -- | Map of data constructor names to their enclosing type declarations.
        , storeDataDefsByDaCon      :: IORef (Map ModuleName (Map n (DataDef n)))

          -- | Map of type synonym names to their declarations.
        , storeTypeDefsByTyCon      :: IORef (Map ModuleName (Map n (Kind n, Type n)))

          -- | Map of capability names to their declarations.
          --   The 'Import' in 'ImportCap' means this is the information a client
          --   module can use to access the declaration. The defining module might
          --   not have imported it itself.
        , storeCapsByName           :: IORef (Map ModuleName (Map n (ImportCap n (Type n))))

          -- | Map of value names to their import declarations.
          --   The 'Import' in 'ImportValue' means this is the information a client
          --   module can use to access the declaration. The defining module might
          --   not have imported it itself.
        , storeValuesByName         :: IORef (Map ModuleName (Map n (ImportValue n (Type n))))

          -- Complete Interfaces ----------------
          -- | Fully loaded interface files.
          --   In future we want to load parts of interface files on demand,
          --   and not the whole lot.
        , storeInterfaces           :: IORef [Interface n]
        }


---------------------------------------------------------------------------------------------------
-- | Metadata for interfaces currently loaded into the store.
data Meta
        = Meta
        { metaModuleName        :: ModuleName
        , metaFilePath          :: FilePath
        , metaTimeStamp         :: UTCTime }
        deriving Show


---------------------------------------------------------------------------------------------------
-- | Module interface.
data Interface n
        = Interface
        { -- | Path that the interface was loaded from.
          interfaceFilePath     :: FilePath

          -- | Last modification time of the interface file,
          --   used to determine when the source needs to be rebuilt.
        , interfaceTimeStamp    :: UTCTime

        , interfaceVersion      :: String
        , interfaceModuleName   :: ModuleName
        , interfaceModule       :: Module () n }

