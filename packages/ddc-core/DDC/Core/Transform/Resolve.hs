
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

        tyCons  = Map.unions 
                $ map exportedTyCons $ Map.elems deps

        daVars  = Map.unions 
                $ map exportedDaVars $ Map.elems deps


        moreImportTypes
         = catMaybes
         $ [ case Map.lookup n tyCons of
                Just (_modName, k) -> Just (n, ImportSourceAbstract k)
                _                 -> Nothing
           | UName n         <- Set.toList $ supportTyCon sp ]

        moreImportValues
         = catMaybes
         $ [ case Map.lookup n daVars of
                Just (modName, t) -> Just (n, ImportSourceModule modName n t)
                _                 -> Nothing
           | UName n         <- Set.toList $ supportDaVar sp ]

   in   mm { moduleImportTypes   
                = moduleImportTypes  mm ++ moreImportTypes

           , moduleImportValues  
                = moduleImportValues mm ++ moreImportValues  

           , moduleDataDefsLocal 
                =  moduleDataDefsLocal mm 
                ++ (concat $ map moduleDataDefsLocal $ Map.elems deps) }


exportedTyCons
        :: Ord n
        => Module b n -> Map n (ModuleName, Kind n)

exportedTyCons mm
        = Map.fromList
        $ [ (n, (moduleName mm, t)) 
                | (n, ExportSourceLocal _ t) <- moduleExportTypes mm ]


exportedDaVars 
        :: Ord n
        => Module b n -> Map n (ModuleName, Type n)

exportedDaVars mm
        = Map.fromList
        $ [ (n, (moduleName mm, t)) 
                | (n, ExportSourceLocal _ t) <- moduleExportValues mm ]
