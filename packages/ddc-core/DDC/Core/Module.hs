
module DDC.Core.Module
        ( ModuleName    (..)
        , QualName      (..)
        , Module        (..)
        , Extern        (..))
where
import DDC.Core.Exp
import Data.Map         (Map)


-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord)


-- | A fully qualified name, 
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show


-- | A module can be mutually recursive with other modules.
data Module a n

        -- | A module containing core bindings.
        = ModuleCore
        { -- | Name of this module.
          moduleName            :: ModuleName

          -- Exports ------------------
          -- | Kinds of exported type names.
        , moduleExportKinds     :: Map n (Kind n)

          -- | Types of exported value names.
        , moduleExportTypes     :: Map n (Type n)

          -- Imports ------------------
          -- | Map of external value names used in this module,
          --   to their qualified name and types.
        , moduleImportKinds     :: Map n (QualName n, Kind n)

          -- | Map of external value names used in this module,
          --   to their qualified name and types.
        , moduleImportTypes     :: Map n (QualName n, Type n)

          -- Local --------------------
          -- | Non-recursive bindings defined in this module, 
          --   which are initialised on startup.
        , moduleLets            :: [Lets a n] 
        }
        deriving Show


-- | Definition of some external thing.
data Extern n
        -- | Import a function from Sea land.
        = ExternSeaFun
        { -- | Name of the external function.
          externSeaName         :: String

          -- | Type of the function.
        , externType            :: Type n }
