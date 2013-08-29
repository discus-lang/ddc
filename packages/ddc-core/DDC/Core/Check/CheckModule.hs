
module DDC.Core.Check.CheckModule
        ( checkModule
        , checkModuleM)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Check.CheckExp
import DDC.Core.Check.Error
import DDC.Type.Compounds
import DDC.Type.DataDef
import DDC.Type.Equiv
import DDC.Base.Pretty
import DDC.Type.Env             (KindEnv, TypeEnv)
import DDC.Control.Monad.Check  (result, throw)
import Data.Map                 (Map)
import qualified DDC.Type.Check as T
import qualified DDC.Type.Env   as Env
import qualified Data.Map       as Map


-- Wrappers -------------------------------------------------------------------
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
        -> Either (Error a n) (Module (AnTEC a n) n)

checkModule !config !xx 
        = result 
        $ checkModuleM 
                config 
                (configPrimKinds config)
                (configPrimTypes config)
                xx


-- checkModule ----------------------------------------------------------------
-- | Like `checkModule` but using the `CheckM` monad to handle errors.
checkModuleM 
        :: (Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting kind environment.
        -> TypeEnv n            -- ^ Starting type environment.
        -> Module a n           -- ^ Module to check.
        -> CheckM a n (Module (AnTEC a n) n)

checkModuleM !config !kenv !tenv mm@ModuleCore{}
 = do   
        -- Convert the imported kind and type map to a list of binds.
        let bksImport  = [BName n k |  (n, (_, k)) <- Map.toList $ moduleImportKinds mm]
        let btsImport  = [BName n t |  (n, (_, t)) <- Map.toList $ moduleImportTypes mm]

        -- Check the imported kinds and types.
        --  The imported types are in scope in both imported and exported signatures.
        mapM_ (checkTypeM config kenv) $ map typeOfBind bksImport
        let kenv' = Env.union kenv $ Env.fromList bksImport

        mapM_ (checkTypeM config kenv') $ map typeOfBind btsImport
        let tenv' = Env.union tenv $ Env.fromList btsImport

        -- Check the sigs for exported things.
        mapM_ (checkTypeM config kenv') $ Map.elems $ moduleExportKinds mm
        mapM_ (checkTypeM config kenv') $ Map.elems $ moduleExportTypes mm
                
        
        -- TODO: Check the data type definitions.
        --       The constructor types need to return the defined data type.
        let defs'   = unionDataDefs
                        (configDataDefs config)
                        (fromListDataDefs (Map.elems (moduleDataDefsLocal mm)))

        -- Binders for the data type constructors defined by the data defs.
        let bsData  = [BName (dataDefTypeName def) (kindOfDataDef def)
                                | def <- Map.elems (moduleDataDefsLocal mm) ]
        
        let kenv_data   = Env.union kenv' (Env.fromList bsData)                    
        let config_data = config { configDataDefs = defs' }

        -- Check the body of the module.
        (x', _, _effs, _) <- checkExpM config_data kenv_data tenv' (moduleBody mm)

        -- Check that each exported signature matches the type of its binding.
        envDef  <- checkModuleBinds (moduleExportKinds mm) (moduleExportTypes mm) x'

        -- Check that all exported bindings are defined by the module.
        mapM_ (checkBindDefined envDef) $ Map.keys $ moduleExportTypes mm

        -- Return the checked bindings as they have explicit type annotations.
        let mm'         = mm { moduleBody = x' }
        return mm'


-- | Check that the exported signatures match the types of their bindings.
checkModuleBinds 
        :: Ord n
        => Map n (Kind n)               -- ^ Kinds of exported types.
        -> Map n (Type n)               -- ^ Types of exported values.
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

        XLet _ (LLetRegions _ _) x2
         ->     checkModuleBinds ksExports tsExports x2

        _ ->    return Env.empty


-- | If some bind is exported, then check that it matches the exported version.
checkModuleBind 
        :: Ord n
        => Map n (Kind n)       -- ^ Kinds of exported types.
        -> Map n (Type n)       -- ^ Types of exported values.
        -> Bind n
        -> CheckM a n ()

checkModuleBind !_ksExports !tsExports !b
 | BName n tDef <- b
 = case Map.lookup n tsExports of
        Nothing                 -> return ()
        Just tExport 
         | equivT tDef tExport  -> return ()
         | otherwise            -> throw $ ErrorExportMismatch n tExport tDef

 -- Only named bindings can be exported, 
 --  so we don't need to worry about non-named ones.
 | otherwise
 = return ()


-- | Check that a top-level binding is actually defined by the module.
checkBindDefined 
        :: Ord n
        => TypeEnv n            -- ^ Types defined by the module.
        -> n                    -- ^ Name of an exported binding.
        -> CheckM a n ()

checkBindDefined env n
 = case Env.lookup (UName n) env of
        Just _  -> return ()
        _       -> throw $ ErrorExportUndefined n


-------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> KindEnv n 
           -> Type n 
           -> CheckM a n (Kind n)

checkTypeM !config !kenv !tt
 = case T.checkType config kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

