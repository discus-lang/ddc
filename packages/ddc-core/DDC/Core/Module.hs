
module DDC.Core.Module
        ( -- * Modules
          Module        (..)
        , isMainModule
	, moduleKindEnv
        , moduleTypeEnv
        , moduleTopBinds
        , modulesGetBinds

	  -- * Module maps
	, ModuleMap
	, modulesExportTypes
	, modulesExportValues

         -- * Module Names
        , QualName      (..)
        , ModuleName    (..)
        , isMainModuleName

        -- * Import Sources
        , ImportSource  (..))
where
import DDC.Core.Exp
import DDC.Type.DataDef
import DDC.Type.Compounds
import Data.Typeable
import Data.Map.Strict                  (Map)
import Data.Set                         (Set)
import DDC.Type.Env                     as Env
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import Control.DeepSeq
import Data.Maybe


-- Module -----------------------------------------------------------------------------------------
-- | A module can be mutually recursive with other modules.
data Module a n
        = ModuleCore
        { -- | Name of this module.
          moduleName                    :: !ModuleName

          -- Exports ------------------
          -- | Kinds of exported types.
        , moduleExportTypes             :: !(Map n (Kind n))

          -- | Types of exported values.
        , moduleExportValues            :: !(Map n (Type n))

          -- Imports ------------------
          -- | Kinds of imported types,  along with the name of the module they are from.
          --   These imports come from a Disciple module, that we've compiled ourself.
        , moduleImportTypes             :: ![(n, (ImportSource n, Kind n))]

          -- | Types of imported values, along with the name of the module they are from.
          --   These imports come from a Disciple module, that we've compiled ourself.
        , moduleImportValues            :: ![(n, (ImportSource n, Type n))]

          -- Local --------------------
          -- | Data types defined in this module.
        , moduleDataDefsLocal           :: ![DataDef n]

          -- | The module body consists of some let-bindings wrapping a unit
          --   data constructor. We're only interested in the bindings, with
          --   the unit being just a place-holder.
        , moduleBody                    :: !(Exp a n)
        }
        deriving (Show, Typeable)


instance (NFData a, NFData n) => NFData (Module a n) where
 rnf !mm
        =     rnf (moduleName mm)
        `seq` rnf (moduleExportTypes   mm)
        `seq` rnf (moduleExportValues  mm)
        `seq` rnf (moduleImportTypes   mm)
        `seq` rnf (moduleImportValues  mm)
        `seq` rnf (moduleDataDefsLocal mm)
        `seq` rnf (moduleBody mm)


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
        $ [BName n k | (n, (_, k)) <- moduleImportTypes mm]


-- | Get the top-level type environment of a module,
--   from its imported values.
moduleTypeEnv :: Ord n => Module a n -> TypeEnv n
moduleTypeEnv mm
        = Env.fromList 
        $ [BName n k | (n, (_, k)) <- moduleImportValues mm]


-- | Get the set of top-level value bindings in a module.
moduleTopBinds :: Ord n => Module a n -> Set n
moduleTopBinds mm
 = go (moduleBody mm)
 where  go xx
         = case xx of
                XLet _ (LLet (BName n _) _) x2    
                 -> Set.insert n (go x2)

                XLet _ (LLet _ _) x2    
                 -> go x2

                XLet _ (LRec bxs) x2    
                 ->          Set.fromList (mapMaybe takeNameOfBind $ map fst bxs)
                 `Set.union` go x2

                _ -> Set.empty


-- ModuleMap --------------------------------------------------------------------------------------
-- | Map of module names to modules.
type ModuleMap a n 
        = Map ModuleName (Module a n)

modulesGetBinds m 
        = Env.fromList $ map (uncurry BName) (Map.assocs m)


-- | Add the kind environment exported by all these modules to the given one.
modulesExportTypes :: Ord n => ModuleMap a n -> KindEnv n -> KindEnv n
modulesExportTypes mods base
        = foldl Env.union base 
        $ map (modulesGetBinds.moduleExportTypes) (Map.elems mods)


-- | Add the type environment exported by all these modules to the given one.
modulesExportValues :: Ord n => ModuleMap a n -> TypeEnv n -> TypeEnv n
modulesExportValues mods base
        = foldl Env.union base 
        $ map (modulesGetBinds.moduleExportValues) (Map.elems mods)


-- ModuleName -------------------------------------------------------------------------------------
-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord, Typeable)

instance NFData ModuleName where
 rnf (ModuleName ss)
        = rnf ss
 

-- | A fully qualified name, 
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show

instance NFData n => NFData (QualName n) where
 rnf (QualName mn n)
        = rnf mn `seq` rnf n


-- | Check whether this is the name of the \"Main\" module.
isMainModuleName :: ModuleName -> Bool
isMainModuleName mn
 = case mn of
        ModuleName ["Main"]     -> True
        _                       -> False


-- ImportSource -----------------------------------------------------------------------------------
-- | Source of some imported thing.
data ImportSource n
        -- | A type imported abstractly.
        --   It may be defined in a foreign language, but the Disciple program
        --   treats it abstractly.
        = ImportSourceAbstract

        -- | Something imported from a Disciple module that we compiled ourself.
        | ImportSourceModule
        { importSourceModuleName        :: ModuleName 
        , importSourceModuleVar         :: n }

        -- | Something imported via the C calling convention.
        | ImportSourceSea
        { importSourceSeaVar            :: String }
        deriving (Show, Eq)

instance NFData n => NFData (ImportSource n) where
 rnf is
  = case is of
        ImportSourceAbstract    -> ()
        ImportSourceModule mn n -> rnf mn `seq` rnf n
        ImportSourceSea v       -> rnf v

