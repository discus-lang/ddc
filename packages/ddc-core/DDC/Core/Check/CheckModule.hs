
module DDC.Core.Check.CheckModule
        ( checkModule
        , checkModuleM)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Check.CheckExp
import DDC.Core.Check.Error
import DDC.Type.Compounds
import DDC.Base.Pretty
import DDC.Type.Env             (KindEnv, TypeEnv)
import DDC.Type.Check.Monad     (result, throw)
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

checkModule config xx 
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

checkModuleM config kenv tenv mm@ModuleCore{}
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
                
        -- Check our let bindings.
        (x', _, _effs, _) <- checkExpM config kenv' tenv' (moduleBody mm)

        -- TODO: check that types of bindings match types of exports.
        -- TODO: check that all exported bindings are defined.
        -- TODO: don't permit top-level let-bindings to have visible effects.

        -- Return the checked bindings as they have explicit type annotations.
        let mm'         = mm { moduleBody = x' }
        return mm'


-------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> KindEnv n 
           -> Type n 
           -> CheckM a n (Kind n)

checkTypeM config kenv tt
 = case T.checkType (configPrimDataDefs config) kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

