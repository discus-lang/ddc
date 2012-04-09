
module DDC.Core.Check.CheckModule
        ( checkModule
        , checkModuleM)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Check.CheckExp
import DDC.Core.Check.Error
import DDC.Type.DataDef
import DDC.Type.Compounds
import DDC.Base.Pretty
import DDC.Type.Env             (Env)
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
        :: (Ord n, Pretty n)
        => DataDefs n           -- ^ Primitive data type definitions.
        -> Env n                -- ^ Primitive kind environment.
        -> Env n                -- ^ Primitive type environment.
        -> Module a n           -- ^ Module to check.
        -> Either (Error a n) (Module a n)

checkModule defs kenv tenv xx 
 = result $ checkModuleM defs kenv tenv xx


-- checkModule ----------------------------------------------------------------
-- | Like `checkModule` but using the `CheckM` monad to handle errors.
checkModuleM 
        :: (Ord n, Pretty n)
        => DataDefs n           -- ^ Primitive data type definitions.
        -> Env n                -- ^ Primitive kind environment.
        -> Env n                -- ^ Primitive type environment.
        -> Module a n           -- ^ Module to check.
        -> CheckM a n (Module a n)

checkModuleM defs kenv tenv mm@ModuleCore{}
 = do   
        -- Check the sigs for exported things.
        mapM_ (checkTypeM defs kenv) $ Map.elems $ moduleExportKinds mm
        mapM_ (checkTypeM defs kenv) $ Map.elems $ moduleExportTypes mm

        -- Convert the imorted kind and type map to a list of binds.
        let bksImport  = [BName n k |  (n, (_, k)) <- Map.toList $ moduleImportKinds mm]
        let btsImport  = [BName n t |  (n, (_, t)) <- Map.toList $ moduleImportTypes mm]

        -- Check the imported kinds and types.
        mapM_ (checkTypeM defs kenv) $ map typeOfBind bksImport
        mapM_ (checkTypeM defs kenv) $ map typeOfBind btsImport


        -- Build the compound environments.
        -- These contain primitive types as well as the imported ones.
        let kenv'        = Env.extend (BAnon kData)
                         $ Env.union kenv $ Env.fromList bksImport

        let tDummy       = TVar (UIx 0 kData)                                   -- TODO: why did we need this?
        let tenv'        = Env.extend (BAnon tDummy)
                         $ Env.union tenv $ Env.fromList btsImport
                
        -- Check our let bindings (with the dummy expression on the end)
        (x', _, _effs, _) <- checkExpM defs kenv' tenv' (moduleBody mm)

        -- TODO: check that types of bindings match types of exports.
        -- TODO: check that all exported bindings are defined.
        -- TODO: don't permit top-level let-bindings to have visible effects.

        -- Return the checked bindings as they have explicit type annotations.
        let mm'         = mm { moduleBody = x' }

        return mm'


-------------------------------------------------------------------------------
-- | Check a type in the exp checking monad.
checkTypeM :: (Ord n, Pretty n) 
           => DataDefs n 
           -> Env n 
           -> Type n 
           -> CheckM a n (Kind n)

checkTypeM defs kenv tt
 = case T.checkType defs kenv tt of
        Left err        -> throw $ ErrorType err
        Right k         -> return k

