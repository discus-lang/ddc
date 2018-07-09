
module DDC.Core.Check.Judge.Module.Binds where
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Module
import DDC.Core.Env.EnvX                        (EnvX)
import DDC.Core.Check.Context.Oracle            (Oracle)
import DDC.Control.CheckIO                      (throw)
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified DDC.Core.Check.Context.Oracle  as Oracle

---------------------------------------------------------------------------------------------------
-- | Check that the exported signatures match the types of their bindings.
checkModuleBinds
        :: Ord n
        => EnvX n                               -- ^ Starting environment.
        -> [(n, ExportType n (Type n))]         -- ^ Exported types.
        -> [(n, ExportValue n (Type n))]        -- ^ Exported values
        -> Exp (AnTEC a n) n
        -> CheckM a n (EnvX n)                  -- ^ Environment of top-level bindings
                                                --   defined by the module

checkModuleBinds !env !ksExports !tsExports !xx
 = case xx of
        XLet _ (LLet b _) x2
         -> do  checkModuleBind (EnvX.envxEnvT env) ksExports tsExports b
                env'    <- checkModuleBinds env ksExports tsExports x2
                return  $ EnvX.extendX b env'

        XLet _ (LRec bxs) x2
         -> do  mapM_ (checkModuleBind (EnvX.envxEnvT env) ksExports tsExports) $ map fst bxs
                env'    <- checkModuleBinds env ksExports tsExports x2
                return  $ EnvX.extendsX (map fst bxs) env'

        XLet _ (LPrivate _ _ _) x2
         ->     checkModuleBinds env ksExports tsExports x2

        _ ->    return env


-- | If some bind is exported, then check that it matches the exported version.
checkModuleBind
        :: Ord n
        => EnvT n                               -- ^ Environment of types.
        -> [(n, ExportType  n (Type n))]        -- ^ Exported types.
        -> [(n, ExportValue n (Type n))]        -- ^ Exported values.
        -> Bind n
        -> CheckM a n ()

checkModuleBind env !_ksExports !tsExports !b
 | BName n tDef <- b
 = case join $ liftM takeTypeOfExportValue $ lookup n tsExports of
        Nothing                 -> return ()
        Just tExport
         | equivT env tDef tExport  -> return ()
         | otherwise                -> throw $ ErrorExportMismatch n tExport tDef

 -- Only named bindings can be exported,
 --  so we don't need to worry about non-named ones.
 | otherwise
 = return ()


---------------------------------------------------------------------------------------------------
-- | Check that an exported top-level value is actually defined by the module.
checkBindDefined
        :: (Show n, Ord n)
        => EnvX n               -- ^ Environment containing binds defined by the module.
        -> Maybe (Oracle n)     -- ^ Import oracle.
        -> n                    -- ^ Name of an exported binding.
        -> CheckM a n ()

checkBindDefined envx mOracle n
 | Just _       <- EnvX.lookupX (UName n) envx
 = return ()

 | Just oracle  <- mOracle
 = do   mt      <- Oracle.resolveValueName oracle n
        case mt of
         Just _  -> return ()
         Nothing -> throw $ ErrorExportUndefined n

 | otherwise
 = throw $ ErrorExportUndefined n

