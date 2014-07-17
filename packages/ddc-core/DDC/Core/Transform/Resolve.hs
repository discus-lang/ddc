
module DDC.Core.Transform.Resolve
        ( resolveNamesInModule )
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Collect.Support
import DDC.Type.Env             (KindEnv, TypeEnv)
import Data.Maybe
import Data.Map                 (Map)
import qualified Data.Map       as Map
import qualified Data.Set       as Set


-- | For all the names that are free in this module, if there is a
--   corresponding export in one of the modules in the given map,
--   then add the appropriate import definition.
resolveNamesInModule 
        :: Ord n
        => KindEnv n
        -> TypeEnv n
        -> Map ModuleName (Module b n)
        -> Module a n
        -> Module a n

resolveNamesInModule kenv tenv deps mm
 = let
        sp      = support kenv tenv mm

   in   mm { moduleImportTypes   
                =  moduleImportTypes  mm 
                ++ importsForTyCons deps (Set.toList $ supportTyCon sp)

           , moduleImportValues  
                =  moduleImportValues mm 
                ++ importsForDaVars deps (Set.toList $ supportDaVar sp)

           , moduleImportDataDefs
                =  moduleImportDataDefs mm 
                ++ [(def, moduleName m) | m   <- Map.elems deps
                                        , def <- moduleDataDefsLocal m ] }


---------------------------------------------------------------------------------------------------
-- | Build import statements for the given list of unbound type constructors.
--
--   We look in the dependency modules for a matching export,
--   and produce the correspondig import statement to use it.
--
importsForTyCons
        :: Ord n
        => Map ModuleName (Module b n)  -- ^ Modules which this one depends on.
        -> [Bound n]                    -- ^ Unbound type constructors to find imports for.
        -> [(n, ImportSource n)]

importsForTyCons deps tyCons
 = let
        -- Type constructors defined locally by each module and exported.
        tyConsLocal
         = Map.unions $ map exportedTyConsLocal $ Map.elems deps

        -- Type constructors imported abstractly by each module and exported.
        tyConsAbs
         = Map.unions $ map importedTyConsAbs   $ Map.elems deps

        findImport n
         | Just (_, k)      <- Map.lookup n tyConsLocal
         = Just (n, ImportSourceAbstract k)

         | Just k           <- Map.lookup n tyConsAbs
         = Just (n, ImportSourceAbstract k)

         | otherwise
         = Nothing

   in   catMaybes [ findImport n | UName n <- tyCons ]


---------------------------------------------------------------------------------------------------
-- | Build import statements for the given list of unbound value variables.
--
--   We look in dependency modules for a matching export, 
--   and produce the corresponding import statement to use it.
--
importsForDaVars 
        :: Ord n
        => Map ModuleName (Module b n)  -- ^ Modules which this one depends on.
        -> [Bound n]                    -- ^ Unbound type constructors to find imports for.
        -> [(n, ImportSource n)]

importsForDaVars deps daVars
 = let
        -- Variables defined locally by each module and exported.
        daVarsLocal     
         = Map.unions $ map exportedDaVarsLocal $ Map.elems deps

        -- Variables imported by each module via the C calling convention.
        -- TODO: Don't auto-export all foreign imported values.
        daVarsForeign
         = Map.unions $ map importedDaVarsSea   $ Map.elems deps

        findImport n
         | Just (modName, t) <- Map.lookup n daVarsLocal
         = Just (n, ImportSourceModule modName n t)

         | Just (s, t)       <- Map.lookup n daVarsForeign
         = Just (n, ImportSourceSea    s t)

         | otherwise
         = Nothing

   in   catMaybes [ findImport n | UName n <- daVars ]


---------------------------------------------------------------------------------------------------
-- | Get the tycons that are locally defined, then exported by a module.
exportedTyConsLocal :: Ord n => Module b n -> Map n (ModuleName, Kind n)
exportedTyConsLocal mm
        = Map.fromList
        $ [ (n, (moduleName mm, t)) 
                | (n, ExportSourceLocal _ t) <- moduleExportTypes mm ]


-- | Get the data variable names that are locally defined, then exported by a module.
exportedDaVarsLocal :: Ord n => Module b n -> Map n (ModuleName, Type n)
exportedDaVarsLocal mm
        = Map.fromList
        $ [ (n, (moduleName mm, t)) 
                | (n, ExportSourceLocal _ t) <- moduleExportValues mm ]


-- | Get the type constructors that are imported abstractly by a module.
importedTyConsAbs  :: Ord n => Module b n -> Map n (Type n)
importedTyConsAbs mm
        = Map.fromList
        $ [ (n, k)
                | (n, ImportSourceAbstract k)  <- moduleImportTypes mm ]


-- | Get the data variables that are imported from C land by a module.
importedDaVarsSea  :: Ord n => Module b n -> Map n (String, Type n)
importedDaVarsSea mm
        = Map.fromList
        $ [ (n, (s, t))
                | (n, ImportSourceSea s t)     <- moduleImportValues mm ]

