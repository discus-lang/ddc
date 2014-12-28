
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

         -- * Export Sources
        , ExportSource  (..)
        , takeTypeOfExportSource
        , mapTypeOfExportSource

         -- * Import Sources
        , ImportSource  (..)
        , typeOfImportSource
        , mapTypeOfImportSource)
where
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
          moduleName                    :: !ModuleName

          -- | Whether this is a module header only.
          --   Module headers contain type definitions, as well as imports and exports, 
          --   but no function definitions. Module headers are used in interface files.
        , moduleIsHeader                :: !Bool

          -- Exports ------------------
          -- | Kinds of exported types.
        , moduleExportTypes             :: ![(n, ExportSource n)]

          -- | Types of exported values.
        , moduleExportValues            :: ![(n, ExportSource n)]

          -- Imports ------------------
          -- | Kinds of imported types,  along with the name of the module they are from.
          --   These imports come from a Disciple module, that we've compiled ourself.
        , moduleImportTypes             :: ![(n, ImportSource n)]

          -- | Types of imported values, along with the name of the module they are from.
          --   These imports come from a Disciple module, that we've compiled ourself.
        , moduleImportValues            :: ![(n, ImportSource n)]

          -- | Data defs imported from other modules.
        , moduleImportDataDefs          :: ![DataDef n]

          -- Local defs ---------------
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
        =     rnf (moduleName           mm)
        `seq` rnf (moduleIsHeader       mm)
        `seq` rnf (moduleExportTypes    mm)
        `seq` rnf (moduleExportValues   mm)
        `seq` rnf (moduleImportTypes    mm)
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
        $ [BName n (typeOfImportSource isrc) | (n, isrc) <- moduleImportTypes mm]


-- | Get the top-level type environment of a module,
--   from its imported values.
moduleTypeEnv :: Ord n => Module a n -> TypeEnv n
moduleTypeEnv mm
        = Env.fromList 
        $ [BName n (typeOfImportSource isrc) | (n, isrc) <- moduleImportValues mm]


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


-- ModuleName -------------------------------------------------------------------------------------
-- | A hierarchical module name.
data ModuleName
        = ModuleName [String]
        deriving (Show, Eq, Ord, Typeable)

instance NFData ModuleName where
 rnf (ModuleName ss)
        = rnf ss


-- | Read a string like 'M1.M2.M3' as a module name.
readModuleName :: String -> Maybe ModuleName
readModuleName []       = Nothing
readModuleName str
 = Just $ ModuleName $ go str
 where
        go s
         | elem '.' s
         , (n, '.' : rest)      <- span (/= '.') s
         = n : go rest

         | otherwise
         = [s]


-- | Check whether this is the name of the \"Main\" module.
isMainModuleName :: ModuleName -> Bool
isMainModuleName mn
 = case mn of
        ModuleName ["Main"]     -> True
        _                       -> False


-- QualName ---------------------------------------------------------------------------------------
-- | A fully qualified name, 
--   including the name of the module it is from.
data QualName n
        = QualName ModuleName n
        deriving Show

instance NFData n => NFData (QualName n) where
 rnf (QualName mn n)
        = rnf mn `seq` rnf n


-- ExportSource -----------------------------------------------------------------------------------
data ExportSource n
        -- | A name defined in this module, with an explicit type.
        = ExportSourceLocal   
        { exportSourceLocalName         :: n 
        , exportSourceLocalType         :: Type n }

        -- | A named defined in this module, without a type attached.
        --   We use this version for source language where we infer the type of
        --   the exported thing.
        | ExportSourceLocalNoType
        { exportSourceLocalName         :: n }
        deriving (Show, Eq)


instance NFData n => NFData (ExportSource n) where
 rnf es
  = case es of
        ExportSourceLocal n t           -> rnf n `seq` rnf t
        ExportSourceLocalNoType n       -> rnf n


-- | Take the type of an imported thing, if there is one.
takeTypeOfExportSource :: ExportSource n -> Maybe (Type n)
takeTypeOfExportSource es
 = case es of
        ExportSourceLocal _ t           -> Just t
        ExportSourceLocalNoType{}       -> Nothing


-- | Apply a function to any type in an ExportSource.
mapTypeOfExportSource :: (Type n -> Type n) -> ExportSource n -> ExportSource n
mapTypeOfExportSource f esrc
 = case esrc of
        ExportSourceLocal n t           -> ExportSourceLocal n (f t)
        ExportSourceLocalNoType n       -> ExportSourceLocalNoType n


-- ImportSource -----------------------------------------------------------------------------------
-- | Source of some imported thing.
data ImportSource n
        -- | A type imported abstractly.
        --   Used for phantom types of kind Data, 
        --   as well as any type that does not have kind Data.
        = ImportSourceAbstract
        { importSourceAbstractType      :: Type n }

        -- | The type of some boxed data which is defined somewhere else.
        --   The objects follow the standard heap object layout, but the code
        --   that constructs and destructs them may have been written in a 
        --   different language.
        --   Used when importing data types defined in Salt modules.
        | ImportSourceBoxed
        { importSourceBoxed             :: Kind n }

        -- | Something imported from a Disciple module that we compiled ourself.
        | ImportSourceModule
        { importSourceModuleName        :: ModuleName 
        , importSourceModuleVar         :: n 
        , importSourceModuleType        :: Type n }

        -- | Something imported via the C calling convention.
        | ImportSourceSea
        { importSourceSeaVar            :: String 
        , importSourceSeaType           :: Type n }
        deriving (Show, Eq)


instance NFData n => NFData (ImportSource n) where
 rnf is
  = case is of
        ImportSourceAbstract t          -> rnf t
        ImportSourceBoxed t             -> rnf t
        ImportSourceModule mn n t       -> rnf mn `seq` rnf n `seq` rnf t
        ImportSourceSea v t             -> rnf v  `seq` rnf t


-- | Take the type of an imported thing.
typeOfImportSource :: ImportSource n -> Type n
typeOfImportSource src
 = case src of
        ImportSourceAbstract   t        -> t
        ImportSourceBoxed      t        -> t
        ImportSourceModule _ _ t        -> t
        ImportSourceSea      _ t        -> t


-- | Apply a function to the type in an ImportSource.
mapTypeOfImportSource :: (Type n -> Type n) -> ImportSource n -> ImportSource n
mapTypeOfImportSource f isrc
 = case isrc of
        ImportSourceAbstract  t         -> ImportSourceAbstract (f t)
        ImportSourceBoxed     t         -> ImportSourceBoxed    (f t)
        ImportSourceModule mn n t       -> ImportSourceModule mn n (f t)
        ImportSourceSea s t             -> ImportSourceSea s (f t)


