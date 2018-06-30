
module DDC.Core.Interface.Base
        ( Store         (..)
        , Meta          (..)
        , Interface     (..)
        , Super         (..))
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
--   This lives in IO land because in future we want to demand-load the
--   inferface files as needed, rather than loading the full dependency
--   tree. Keeping it in IO means that callers must also be in IO.
--
data Store n
        = Store
        { -- | Metadata for interface files currently represented in the store.
          storeMeta             :: IORef [Meta]

          -- Name Resolution --------------------
          -- These maps are used when resolving qualified names to determine which module
          -- a name is defined in.

          -- | Map of type constructor names to modules that define one of that name.
        , storeTypeCtorNames    :: IORef (Map n (Set ModuleName))

          -- | Map of data constructor names to modules that define one of that name.
        , storeDataCtorNames    :: IORef (Map n (Set ModuleName))

          -- Module Data -----------------------
          -- These maps contain the top level declarations of a module in unpacked
          -- form. We use an outer map by ModuleName rather than having a single
          -- key of type (ModuleName, n) so that if the module source code changes
          -- then the stale components loaded for that module can be easily dropped.

          -- | Lookup the definition of the given top-level super,
          --   from one or more of the provided modules.
        , storeSupers           :: IORef (Map ModuleName (Map n (Super n)))

          -- | Map of data type names to their declarations.
        , storeDataDefsByTyCon  :: IORef (Map ModuleName (Map n (DataDef n)))

          -- | Map of data constructor names to their enclosing type declarations.
        , storeDataDefsByDaCon  :: IORef (Map ModuleName (Map n (DataDef n)))

          -- Complete Interfaces ----------------
          -- | Fully loaded interface files.
          --   In future we want to load parts of interface files on demand,
          --   and not the whole lot.
        , storeInterfaces       :: IORef [Interface n] }


---------------------------------------------------------------------------------------------------
-- | Metadata for interfaces currently loaded into the store.
data Meta
        = Meta
        { metaFilePath          :: FilePath
        , metaTimeStamp         :: UTCTime
        , metaModuleName        :: ModuleName }
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
        , interfaceModule       :: Maybe (Module () n) }


---------------------------------------------------------------------------------------------------
-- | Interface for some top-level super.
data Super n
        = Super
        { -- | Name of the super.
          superName             :: n

          -- | Where the super was imported from.
          --
          --   This is the module that the name was resolved from. If that
          --   module re-exported an imported name then this may not be the
          --   module the super was actually defined in.
        , superModuleName       :: ModuleName

          -- | Tetra type for the super.
        , superType             :: Type n

          -- | Import source for the super.
          --
          --   This can be used to refer to the super from a client module.
        , superImportValue      :: ImportValue n (Type n) }

