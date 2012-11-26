
module DDC.Core.Module
        ( -- * Modules
          Module        (..)
        , isMainModule
	, moduleKindEnv
        , moduleTypeEnv

	  -- * Module maps
	, ModuleMap
	, modulesExportKinds
	, modulesExportTypes

         -- * Module Names.
        , QualName      (..)
        , ModuleName    (..)
        , isMainModuleName)
where
import DDC.Core.Exp
import Data.Typeable
import Data.Map.Strict                  (Map)
import DDC.Type.Env                     as Env
import qualified Data.Map.Strict        as Map


-- Module ---------------------------------------------------------------------
-- | A module can be mutually recursive with other modules.
data Module a n
        = ModuleCore
        { -- | Name of this module.
          moduleName            :: !ModuleName

          -- Exports ------------------
          -- | Kinds of exported types.
        , moduleExportKinds     :: !(Map n (Kind n))

          -- | Types of exported values.
        , moduleExportTypes     :: !(Map n (Type n))

          -- Imports ------------------
          -- | Kinds of imported types,
          --   along with the name of the module they are from.
        , moduleImportKinds     :: !(Map n (QualName n, Kind n))

          -- | Types of imported values,
          --   along with the name of the module they are from.
        , moduleImportTypes     :: !(Map n (QualName n, Type n))

          -- Local --------------------
          -- | The module body consists of some let-bindings
          --   wrapping a unit data constructor.
          -- 
          --  We're only interested in the bindings, 
          --  with the unit being just a place-holder.
        , moduleBody            :: !(Exp a n)
        }
        deriving (Show, Typeable)


-- | Check if this is the `Main` module.
isMainModule :: Module a n -> Bool
isMainModule mm
        = isMainModuleName 
        $ moduleName mm


-- | Get the top-level kind environment of a module,
--   from its imported types.
moduleKindEnv :: Ord n => Module a n -> KindEnv n
moduleKindEnv mm
        = Env.fromList 
        $ [BName n k | (n, (_, k)) <- Map.toList $ moduleImportKinds mm]


-- | Get the top-level type environment of a module,
--   from its imported values.
moduleTypeEnv :: Ord n => Module a n -> TypeEnv n
moduleTypeEnv mm
        = Env.fromList 
        $ [BName n k | (n, (_, k)) <- Map.toList $ moduleImportTypes mm]


-- ModuleMap ------------------------------------------------------------------
-- | Map of module names to modules.
type ModuleMap a n 
        = Map ModuleName (Module a n)

modulesGetBinds m 
        = Env.fromList $ map (uncurry BName) (Map.assocs m)


-- | Add the kind environment exported by all these modules to the given one.
modulesExportKinds :: Ord n => ModuleMap a n -> KindEnv n -> KindEnv n
modulesExportKinds mods base
        = foldl Env.union base 
        $ map (modulesGetBinds.moduleExportKinds) (Map.elems mods)


-- | Add the type environment exported by all these modules to the given one.
modulesExportTypes :: Ord n => ModuleMap a n -> TypeEnv n -> TypeEnv n

modulesExportTypes mods base
        = foldl Env.union base 
        $ map (modulesGetBinds.moduleExportTypes) (Map.elems mods)


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


-- | Check whether this is the name of the \"Main\" module.
isMainModuleName :: ModuleName -> Bool
isMainModuleName mn
 = case mn of
        ModuleName ["Main"]     -> True
        _                       -> False

