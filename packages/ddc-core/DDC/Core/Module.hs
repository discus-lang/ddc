
module DDC.Core.Module
        ( -- * Modules
          Module        (..)
        , isMainModule
        , moduleDataDefs
        , moduleKindEnv
        , moduleTypeEnv
        , moduleTopBinds
        , moduleTopBindTypes
        , mapTopBinds

          -- * Module maps
        , ModuleMap
        , modulesExportTypes
        , modulesExportValues

         -- * Module Names
        , ModuleName    (..)
        , readModuleName
        , isMainModuleName

         -- * Qualified names.
        , QualName      (..)

         -- * Export Definitions
        , ExportSource  (..)
        , takeTypeOfExportSource
        , mapTypeOfExportSource

         -- * Import Definitions
         -- ** Import Types
        , ImportType    (..)
        , kindOfImportType
        , mapKindOfImportType

         -- ** Import Capabilities
        , ImportCap     (..)
        , typeOfImportCap
        , mapTypeOfImportCap

         -- ** Import Types
        , ImportValue   (..)
        , typeOfImportValue
        , mapTypeOfImportValue)
where
import DDC.Core.Module.Export
import DDC.Core.Module.Import
import DDC.Core.Module.Name
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.DataDef                 
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
          moduleName            :: !ModuleName

          -- | Whether this is a module header only.
          --   Module headers contain type definitions, as well as imports and exports, 
          --   but no function definitions. Module headers are used in interface files.
        , moduleIsHeader        :: !Bool

          -- Exports ------------------
          -- | Kinds of exported types.
        , moduleExportTypes     :: ![(n, ExportSource n)]

          -- | Types of exported values.
        , moduleExportValues    :: ![(n, ExportSource n)]

          -- Imports ------------------
          -- | Define imported types.
        , moduleImportTypes     :: ![(n, ImportType  n)]

          -- | Define imported capabilities.
        , moduleImportCaps      :: ![(n, ImportCap n)]

          -- | Define imported values.
        , moduleImportValues    :: ![(n, ImportValue n)]

          -- | Data defs imported from other modules.
        , moduleImportDataDefs  :: ![DataDef n]

          -- Local defs ---------------
          -- | Data types defined in this module.
        , moduleDataDefsLocal   :: ![DataDef n]

          -- | The module body consists of some let-bindings wrapping a unit
          --   data constructor. We're only interested in the bindings, with
          --   the unit being just a place-holder.
        , moduleBody            :: !(Exp a n)
        }
        deriving (Show, Typeable)


instance (NFData a, NFData n) => NFData (Module a n) where
 rnf !mm
        =     rnf (moduleName           mm)
        `seq` rnf (moduleIsHeader       mm)
        `seq` rnf (moduleExportTypes    mm)
        `seq` rnf (moduleExportValues   mm)
        `seq` rnf (moduleImportTypes    mm)
        `seq` rnf (moduleImportCaps     mm)
        `seq` rnf (moduleImportValues   mm)
        `seq` rnf (moduleImportDataDefs mm)
        `seq` rnf (moduleDataDefsLocal  mm)
        `seq` rnf (moduleBody           mm)


-- | Check if this is the `Main` module.
isMainModule :: Module a n -> Bool
isMainModule mm
        = isMainModuleName 
        $ moduleName mm


-- | Get the data type definitions visible in a module.
moduleDataDefs :: Ord n => Module a n -> DataDefs n
moduleDataDefs mm
        = fromListDataDefs 
        $ (moduleImportDataDefs mm ++ moduleDataDefsLocal mm)


-- | Get the top-level kind environment of a module,
--   from its imported types.
moduleKindEnv :: Ord n => Module a n -> KindEnv n
moduleKindEnv mm
        = Env.fromList 
        $ [BName n (kindOfImportType isrc) | (n, isrc) <- moduleImportTypes mm]


-- | Get the top-level type environment of a module,
--   from its imported values.
moduleTypeEnv :: Ord n => Module a n -> TypeEnv n
moduleTypeEnv mm
        = Env.fromList 
        $ [BName n (typeOfImportValue isrc) | (n, isrc) <- moduleImportValues mm]


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
                 ->     Set.fromList (mapMaybe takeNameOfBind $ map fst bxs)
                 `Set.union` go x2

                _ -> Set.empty


-- | Get a map of named top-level bindings to their types.
moduleTopBindTypes :: Ord n => Module a n -> Map n (Type n)
moduleTopBindTypes mm
 = go Map.empty (moduleBody mm)
 where  go acc xx
         = case xx of
                XLet _ (LLet (BName n t) _) x2
                  -> go (Map.insert n t acc) x2

                XLet _ (LLet _ _) x2
                  -> go acc x2

                XLet _ (LRec bxs) x2
                  -> let nts    = Map.fromList [(n, t) | BName n t <- map fst bxs]
                     in  go (Map.union acc nts) x2
                 
                _ -> acc


-- | Apply a function to all the top-level bindings in a module,
--   producing a list of the results.
mapTopBinds :: (Bind n -> Exp a n -> b) -> Module a n -> [b]
mapTopBinds f mm
 = go [] (moduleBody mm)
 where 
        go acc xx
         = case xx of
                XLet _ (LLet b1 x1) x2
                 -> go (f b1 x1 : acc) x2

                XLet _ (LRec bxs)   x2
                 -> let rs      = reverse $ map (uncurry f) bxs
                    in  go (rs ++ acc)  x2

                _ -> reverse acc


-- ModuleMap --------------------------------------------------------------------------------------
-- | Map of module names to modules.
type ModuleMap a n 
        = Map ModuleName (Module a n)


-- | Add the kind environment exported by all these modules to the given one.
modulesExportTypes :: Ord n => ModuleMap a n -> KindEnv n -> KindEnv n
modulesExportTypes mods base
 = let  envOfModule m
         = Env.fromList
         $ [BName n t   |  (n, Just t) 
                        <- map (liftSnd takeTypeOfExportSource) $ moduleExportTypes m]

        liftSnd f (x, y) = (x, f y)

   in   Env.unions $ base : (map envOfModule $ Map.elems mods)
         

-- | Add the type environment exported by all these modules to the given one.
modulesExportValues :: Ord n => ModuleMap a n -> TypeEnv n -> TypeEnv n
modulesExportValues mods base
 = let  envOfModule m
         = Env.fromList
         $ [BName n t   | (n, Just t)
                        <- map (liftSnd takeTypeOfExportSource) $ moduleExportValues m] 

        liftSnd f (x, y) = (x, f y)

   in   Env.unions $ base : (map envOfModule $ Map.elems mods)

