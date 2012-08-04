
module DDC.Core.Module
        ( -- * Module Names.
          QualName      (..)
        , ModuleName    (..)
        , isMainModuleName

          -- * Modules
        , Module        (..)
        , Extern        (..)
        , isMainModule
	, moduleKindEnv
        , moduleTypeEnv

	  -- * Module maps
	, ModuleMap
	, modulesExportKinds
	, modulesExportTypes)

where
import DDC.Core.Exp
import Data.Typeable
import Data.Map                         (Map)
import DDC.Type.Env                     as Env
import qualified Data.Map               as Map


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


-- | Check if this is the `Main` module.
isMainModule :: Module a n -> Bool
isMainModule mm
        = isMainModuleName 
        $ moduleName mm


-- | Get the top-level kind environment of a module,
--   from its imported types.
moduleKindEnv :: Ord n => Module a n -> Env n
moduleKindEnv mm
        = Env.fromList 
        $ [BName n k | (n, (_, k)) <- Map.toList $ moduleImportKinds mm]


-- | Get the top-level type environment of a module,
--   from its imported values.
moduleTypeEnv :: Ord n => Module a n -> Env n
moduleTypeEnv mm
        = Env.fromList 
        $ [BName n k | (n, (_, k)) <- Map.toList $ moduleImportTypes mm]


-- ModuleMap ------------------------------------------------------------------
-- | Map of module names to modules.
type ModuleMap a n 
        = Map ModuleName (Module a n)

modulesGetBinds m 
        = Env.fromList $ map (uncurry BName) (Map.assocs m)


-- | Get the kind environment exported by the module.
modulesExportKinds
	:: (Eq n, Ord n, Show n)
	=> ModuleMap a n -> Env n -> Env n

modulesExportKinds mods base
 = foldl Env.union base $ map (modulesGetBinds.moduleExportKinds) (Map.elems mods)


-- | Get the type environment exported by the module.
modulesExportTypes
        :: (Eq n, Ord n, Show n)
        => ModuleMap a n -> Env n -> Env n

modulesExportTypes mods base
 = foldl Env.union base $ map (modulesGetBinds.moduleExportTypes) (Map.elems mods)

