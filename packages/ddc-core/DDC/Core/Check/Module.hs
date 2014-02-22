
module DDC.Core.Check.Module
        ( checkModule
        , checkModuleM)
where
import DDC.Core.Check.Base      (checkTypeM)
import DDC.Core.Check.Exp
import DDC.Core.Check.Error
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.MapT
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Type.Check.Context
import DDC.Type.Check.Data
import DDC.Type.Compounds
import DDC.Type.DataDef
import DDC.Type.Equiv
import DDC.Type.Universe
import DDC.Base.Pretty
import DDC.Type.Env             (KindEnv, TypeEnv)
import DDC.Control.Monad.Check  (runCheck, throw)
import Data.Maybe
import Data.Monoid
import Control.Monad
import qualified DDC.Type.Env   as Env
import qualified Data.Map       as Map


-- Wrappers ---------------------------------------------------------------------------------------
-- | Type check a module.
--
--   If it's good, you get a new version with types attached to all the bound
--   variables
--
--   If it's bad, you get a description of the error.
checkModule
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> Module a n           -- ^ Module to check.
        -> Mode n               -- ^ Type checker mode.
        -> ( Either (Error a n) (Module (AnTEC a n) n)
           , CheckTrace )

checkModule !config !xx !mode
 = let  (s, result)     = runCheck (mempty, 0, 0)
                        $ checkModuleM config 
                                (configPrimKinds config)
                                (configPrimTypes config)
                                xx mode
        (tr, _, _)      = s
   in   (result, tr)


-- checkModule ------------------------------------------------------------------------------------
-- | Like `checkModule` but using the `CheckM` monad to handle errors.
checkModuleM 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting kind environment.
        -> TypeEnv n            -- ^ Starting type environment.
        -> Module a n           -- ^ Module to check.
        -> Mode n               -- ^ Type checker mode.
        -> CheckM a n (Module (AnTEC a n) n)

checkModuleM !config !kenv !tenv mm@ModuleCore{} !mode
 = do   
        -- Check kinds of imported types ------------------
        nksImport'      <- checkImportTypes config mode (moduleImportTypes mm)

        -- Build the initial kind environment.
        let kenv'       = Env.union kenv 
                        $ Env.fromList  [ BName n (typeOfImportSource isrc)
                                                | (n, isrc) <- nksImport' ]


        -- Check types of imported values -----------------
        ntsImport'      <- checkImportValues config kenv' mode (moduleImportValues mm)        
        
        -- Build the initial type environment.
        let tenv'       = Env.union tenv 
                        $ Env.fromList  [ BName n (typeOfImportSource isrc)
                                                | (n, isrc) <- ntsImport' ]


        -- Check the sigs of exported types ---------------
        mapM_ (\k -> checkTypeM config kenv' emptyContext UniverseKind k Recon) 
                $ mapMaybe (takeTypeOfExportSource . snd) $ moduleExportTypes mm


        -- Check the sigs of exported values --------------
        mapM_ (\k -> checkTypeM config kenv' emptyContext UniverseSpec k Recon) 
                $ mapMaybe (takeTypeOfExportSource . snd) $ moduleExportValues mm                

        
        -- Check the local data type defs -----------------
        defs'        
         <- case checkDataDefs config (moduleDataDefsLocal mm) of
                (err : _, _)   -> throw $ ErrorData err
                ([], defs')    -> return defs'

        let defs_all =  unionDataDefs (configDataDefs config) 
                                      (fromListDataDefs defs')
                                      
        
        -- Check the body of the module -------------------
        let bsData      = [BName (dataDefTypeName def) (kindOfDataDef def) | def <- defs' ]
        let kenv_data   = Env.union kenv' (Env.fromList bsData)                    
        let config_data = config { configDataDefs = defs_all }

        (x', _, _effs, _, ctx) 
                <- checkExpM 
                        (makeTable config_data kenv_data tenv')
                        emptyContext (moduleBody mm) mode

        -- Apply the final context to the annotations in expressions.
        let applyToAnnot (AnTEC t0 e0 c0 x0)
                = AnTEC (applySolved ctx t0)
                        (applySolved ctx e0)
                        (applySolved ctx c0)
                        x0

        let x'' = reannotate applyToAnnot 
                $ mapT (applySolved ctx) x'


        -- Check that each exported signature matches the type of its binding.
        envDef  <- checkModuleBinds (moduleExportTypes mm) (moduleExportValues mm) x''

        -- Check that all exported bindings are defined by the module.
        mapM_ (checkBindDefined envDef) 
                $ map fst $ moduleExportValues mm

        -- If exported names are missing types then fill them in.
        let tsTop       = moduleTopBindTypes mm

        let updateExportSource e
                | ExportSourceLocalNoType n <- e
                , Just t  <- Map.lookup n tsTop   
                = ExportSourceLocal n t
                
                | otherwise = e

        let ntsExport'
                = [ (n, updateExportSource e) | (n, e) <- moduleExportValues mm ]

                                 
        -- Return the checked bindings as they have explicit type annotations.
        let mm' = mm    
                { moduleExportValues    = ntsExport'
                , moduleImportTypes     = nksImport'
                , moduleImportValues    = ntsImport'
                , moduleBody            = x'' }

        return mm'

---------------------------------------------------------------------------------------------------
-- | Check kinds of imported types.
checkImportTypes
        :: (Ord n, Show n, Pretty n)
        => Config n -> Mode n
        -> [(n, ImportSource n)]
        -> CheckM a n [(n, ImportSource n)]

checkImportTypes config mode nksImport
 = do
        -- Checker mode to use.
        let modeCheckImportTypes
                = case mode of
                        Recon   -> Recon
                        _       -> Synth

        -- Check all the kinds in an empty context.
        (ksImport', _, _)
                <- liftM unzip3 
                $  mapM (\k -> checkTypeM config Env.empty emptyContext UniverseKind 
                                          k modeCheckImportTypes) 
                $  [typeOfImportSource isrc | (_, isrc) <- nksImport]
        
        -- Update the original import list with the checked kinds.
        let nksImport'
                = [ (n, mapTypeOfImportSource (const k') isrc)
                                | (n, isrc)     <- nksImport
                                | k'            <- ksImport' ]
        return nksImport'


---------------------------------------------------------------------------------------------------
-- | Check types of imported values.
checkImportValues
        :: (Ord n, Show n, Pretty n)
        => Config n -> KindEnv n -> Mode n
        -> [(n, ImportSource n)]
        -> CheckM a n [(n, ImportSource n)]

checkImportValues config kenv mode ntsImport
 = do
        -- Checker mode to use.
        let modeCheckImportTypes
                = case mode of
                        Recon   -> Recon
                        _       -> Check kData

        -- Check all the types in an empty context.
        (tsImport', _, _)
                <- liftM unzip3
                $  mapM (\t -> checkTypeM config kenv emptyContext UniverseSpec
                                          t modeCheckImportTypes)
                $  map (typeOfImportSource . snd) ntsImport

        -- Update the original import list with the checked types.
        let ntsImport'
                = [ (n, mapTypeOfImportSource (const t') isrc) 
                        | (n, isrc)     <- ntsImport
                        | t'            <- tsImport' ]

        -- TODO: post-check for data kind in Recon mode.

        return ntsImport'


---------------------------------------------------------------------------------------------------
-- | Check that the exported signatures match the types of their bindings.
checkModuleBinds 
        :: Ord n
        => [(n, ExportSource n)]        -- ^ Exported types.
        -> [(n, ExportSource n)]        -- ^ Exported values
        -> Exp (AnTEC a n) n
        -> CheckM a n (TypeEnv n)       -- ^ Environment of top-level bindings
                                        --   defined by the module

checkModuleBinds !ksExports !tsExports !xx
 = case xx of
        XLet _ (LLet b _) x2     
         -> do  checkModuleBind  ksExports tsExports b
                env     <- checkModuleBinds ksExports tsExports x2
                return  $ Env.extend b env

        XLet _ (LRec bxs) x2
         -> do  mapM_ (checkModuleBind ksExports tsExports) $ map fst bxs
                env     <- checkModuleBinds ksExports tsExports x2
                return  $ Env.extends (map fst bxs) env

        XLet _ (LPrivate _ _ _) x2
         ->     checkModuleBinds ksExports tsExports x2

        _ ->    return Env.empty


-- | If some bind is exported, then check that it matches the exported version.
checkModuleBind 
        :: Ord n
        => [(n, ExportSource n)]        -- ^ Exported types.
        -> [(n, ExportSource n)]        -- ^ Exported values.
        -> Bind n
        -> CheckM a n ()

checkModuleBind !_ksExports !tsExports !b
 | BName n tDef <- b
 = case join $ liftM takeTypeOfExportSource $ lookup n tsExports of
        Nothing                 -> return ()
        Just tExport 
         | equivT tDef tExport  -> return ()
         | otherwise            -> throw $ ErrorExportMismatch n tExport tDef

 -- Only named bindings can be exported, 
 --  so we don't need to worry about non-named ones.
 | otherwise
 = return ()


---------------------------------------------------------------------------------------------------
-- | Check that a top-level binding is actually defined by the module.
checkBindDefined 
        :: Ord n
        => TypeEnv n                    -- ^ Types defined by the module.
        -> n                            -- ^ Name of an exported binding.
        -> CheckM a n ()

checkBindDefined env n
 = case Env.lookup (UName n) env of
        Just _  -> return ()
        _       -> throw $ ErrorExportUndefined n

