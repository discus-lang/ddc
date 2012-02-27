
module DDC.Core.Check.CheckModule
        ( checkModule
        , checkModuleM)
where
import DDC.Core.DataDef
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Check.CheckWitness
import DDC.Core.Check.Error
import DDC.Base.Pretty
import DDC.Type.Env             (Env)
import DDC.Type.Check.Monad     (result)
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

checkModuleM _defs kenv tenv mm@ModuleCore{}
 = do   let _kenv'   = Env.union kenv $ Env.fromList
                    $ [BName n k |  (n, (_, k))
                                 <- Map.toList $ moduleImportKinds mm]

        let _tenv'   = Env.union tenv $ Env.fromList
                    $ [BName n t |  (n, (_, t))
                                 <- Map.toList $ moduleImportTypes mm]

        return mm

checkModuleM _defs _kenv _tenv ModuleForeign{}
 = error "checkModuleM: not finished"


