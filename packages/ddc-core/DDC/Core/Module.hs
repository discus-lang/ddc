
module DDC.Core.Module
        ( -- * Module Names.
          QualName      (..)
        , ModuleName    (..)
        , isMainModuleName

          -- * Modules
        , Module        (..)
        , Extern        (..)
        , isMainModule
	
	  -- * Module maps
	, ModuleMap
	, modulesExportKinds
	, modulesExportTypes)

where
import DDC.Core.Exp
import Data.Typeable
import Data.Map                         (Map)
import qualified Data.Map               as Map
import qualified DDC.Type.Env		as E


-- ModuleName -----------------------------------------------------------------
-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord, Typeable)


-- | A fully qualified name, 
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show


isMainModuleName :: ModuleName -> Bool
isMainModuleName mn
 = case mn of
        ModuleName ["Main"]     -> True
        _                       -> False


-- Module ---------------------------------------------------------------------
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
          -- | The module body consists of some let-bindings
          --   wrapping a hole. We're only interested in the bindings, 
          --   with the hole being just a place-holder.
        , moduleBody            :: Exp a n
        }
        deriving (Show, Typeable)


-- | Definition of some external thing.
data Extern n
        -- | Import a function from Sea land.
        = ExternSeaFun
        { -- | Name of the external function.
          externSeaName         :: String

          -- | Type of the function.
        , externType            :: Type n }


isMainModule :: Module a n -> Bool
isMainModule mm
        = isMainModuleName 
        $ moduleName mm


type ModuleMap a n = Map ModuleName (Module a n)

modulesGetBinds m = E.fromList $ map (uncurry BName) (Map.assocs m)

--modulesExportKinds
--	:: (Eq n, Ord n, Show n)
--	=> 
modulesExportKinds mods base
 = foldl E.union base $ map (modulesGetBinds.moduleExportKinds) (Map.elems mods)

modulesExportTypes mods base
 = foldl E.union base $ map (modulesGetBinds.moduleExportTypes) (Map.elems mods)

