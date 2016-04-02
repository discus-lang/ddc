
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
import DDC.Type.Predicates
import DDC.Type.DataDef
import DDC.Type.Equiv
import DDC.Type.Universe
import DDC.Base.Pretty
import DDC.Type.Env             (KindEnv, TypeEnv)
import DDC.Control.Monad.Check  (runCheck, throw)
import DDC.Data.ListUtils
import Control.Monad
import qualified DDC.Type.Env   as Env


-- Wrappers ---------------------------------------------------------------------------------------
-- | Type check a module.
--
--   If it's good, you get a new version with types attached to all the bound
--   variables
--
--   If it's bad, you get a description of the error.
checkModule
        :: (Show a, Ord n, Show n, Pretty n)
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
        :: (Show a, Ord n, Show n, Pretty n)
        => Config n             -- ^ Static configuration.
        -> KindEnv n            -- ^ Starting kind environment.
        -> TypeEnv n            -- ^ Starting type environment.
        -> Module a n           -- ^ Module to check.
        -> Mode n               -- ^ Type checker mode.
        -> CheckM a n (Module (AnTEC a n) n)

checkModuleM !config !kenv !tenv mm@ModuleCore{} !mode
 = do
        -- Check kinds of imported types ------------------
        nksImported'    <- checkImportTypes config mode
                        $  moduleImportTypes mm


        -- Check imported data type defs ------------------
        let defsImported = moduleImportDataDefs mm
        defsImported'   <- case checkDataDefs config defsImported of
                                (err : _, _)      -> throw $ ErrorData err
                                ([], defsImported') -> return defsImported'


        -- Build the imported defs and kind environment.
        --  This contains kinds of type visible in the imported values.
        let config_import = config
                        { configDataDefs = unionDataDefs (configDataDefs config)
                                                         (fromListDataDefs defsImported') }
        let kenv_import = Env.union kenv
                        $ Env.fromList  [ BName n (kindOfImportType isrc)
                                        | (n, isrc) <- nksImported' ]


        -- Check types of imported capabilities -----------
        ntsImportCap'   <- checkImportCaps config_import kenv_import mode
                        $  moduleImportCaps mm


        -- Check types of imported values -----------------
        ntsImportValue' <- checkImportValues config_import kenv_import mode
                        $  moduleImportValues mm


        -- Check the local data type defs -----------------
        let defsLocal   =  moduleDataDefsLocal mm
        defsLocal'      <- case checkDataDefs config defsLocal of
                                (err : _, _)     -> throw $ ErrorData err
                                ([], defsLocal') -> return defsLocal'


        -- Build the top-level config, defs and environments.
        --  These contain names that are visible to bindings in the module.
        let defs_top    = unionDataDefs (configDataDefs config)
                        $ unionDataDefs (fromListDataDefs defsImported')
                                        (fromListDataDefs defsLocal')

        let config_top  = config { configDataDefs = defs_top }
        let kenv_top    = kenv_import

        let tenv_top    = Env.unions 
                        [ tenv
                        , Env.fromList  [ BName n (typeOfImportValue isrc)
                                        | (n, isrc) <- ntsImportValue' ]
                        ]

        let bsImportCap = [ BName n (typeOfImportCap   isrc)
                          | (n, isrc) <- ntsImportCap' ]

        let ctx_top     = pushTypes bsImportCap emptyContext

        -- Check the sigs of exported types ---------------
        esrcsType'      <- checkExportTypes  config_top
                        $  moduleExportTypes mm


        -- Check the sigs of exported values --------------
        esrcsValue'     <- checkExportValues config_top kenv_top
                        $  moduleExportValues mm


        -- Check the body of the module -------------------
        (x', _, _effs, ctx)
         <- checkExpM   (makeTable config_top kenv_top tenv_top)
                        ctx_top (moduleBody mm) mode

        -- Apply the final context to the annotations in expressions.
        let applyToAnnot (AnTEC t0 e0 _ x0)
                = AnTEC (applySolved ctx t0)
                        (applySolved ctx e0)
                        (tBot kClosure)
                        x0

        let x'' = reannotate applyToAnnot
                $ mapT (applySolved ctx) x'


        -- Build new module with infered annotations ------
        let mm_inferred
                = mm
                { moduleExportTypes     = esrcsType'
                , moduleImportTypes     = nksImported'
                , moduleImportCaps      = ntsImportCap'
                , moduleImportValues    = ntsImportValue'
                , moduleBody            = x'' }


        -- Check that each exported signature matches the type of its binding.
        -- This returns an environment containing all the bindings defined
        -- in the module.
        tenv_binds      <- checkModuleBinds
                                (moduleExportTypes  mm_inferred)
                                (moduleExportValues mm_inferred) x''

        -- Build the environment containing all names that can be exported.
        let tenv_exportable = Env.union tenv_top tenv_binds

        -- Check that all exported bindings are defined by the module,
        --   either directly as bindings, or by importing them from somewhere else.
        --   Header modules don't need to contain the complete set of bindings,
        --   but all other modules do.
        when (not $ moduleIsHeader mm_inferred)
                $ mapM_ (checkBindDefined tenv_exportable)
                $ map fst $ moduleExportValues mm_inferred

        -- If exported names are missing types then fill them in.
        let updateExportSource e
                | ExportSourceLocalNoType n <- e
                , Just t  <- Env.lookup (UName n) tenv_exportable
                = ExportSourceLocal n t

                | otherwise = e

        let esrcsValue_updated
                = [ (n, updateExportSource e) | (n, e) <- esrcsValue' ]

        -- Return the checked bindings as they have explicit type annotations.
        let mm_final
                = mm_inferred
                { moduleExportValues    = esrcsValue_updated }

        return mm_final


---------------------------------------------------------------------------------------------------
-- | Check exported types.
checkExportTypes
        :: (Show n, Pretty n, Ord n)
        => Config n
        -> [(n, ExportSource n)]
        -> CheckM a n [(n, ExportSource n)]

checkExportTypes config nesrcs
 = let  check (n, esrc)
         | Just k          <- takeTypeOfExportSource esrc
         = do   (k', _, _) <- checkTypeM config Env.empty emptyContext UniverseKind k Recon
                return  $ (n, mapTypeOfExportSource (const k') esrc)

         | otherwise
         = return (n, esrc)
   in do
        -- Check for duplicate exports.
        let dups = findDuplicates $ map fst nesrcs
        (case takeHead dups of
          Just n -> throw $ ErrorExportDuplicate n
          _      -> return ())


        -- Check the kinds of the export specs.
        mapM check nesrcs


---------------------------------------------------------------------------------------------------
-- | Check exported types.
checkExportValues
        :: (Show n, Pretty n, Ord n)
        => Config n -> KindEnv n
        -> [(n, ExportSource n)]
        -> CheckM a n [(n, ExportSource n)]

checkExportValues config kenv nesrcs
 = let  check (n, esrc)
         | Just t          <- takeTypeOfExportSource esrc
         = do   (t', _, _) <- checkTypeM config kenv emptyContext UniverseSpec t Recon
                return  $ (n, mapTypeOfExportSource (const t') esrc)

         | otherwise
         = return (n, esrc)

   in do
        -- Check for duplicate exports.
        let dups = findDuplicates $ map fst nesrcs
        (case takeHead dups of
          Just n -> throw $ ErrorExportDuplicate n
          _      -> return ())

        -- Check the types of the exported values.
        mapM check nesrcs


---------------------------------------------------------------------------------------------------
-- | Check kinds of imported types.
checkImportTypes
        :: (Ord n, Show n, Pretty n)
        => Config n -> Mode n
        -> [(n, ImportType n)]
        -> CheckM a n [(n, ImportType n)]

checkImportTypes config mode nisrcs
 = let
        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Synth

        check (n, isrc)
         = do   let k           =  kindOfImportType isrc
                (k', _, _)      <- checkTypeM config Env.empty emptyContext UniverseKind
                                        k modeCheckImportTypes
                return  (n, mapKindOfImportType (const k') isrc)
   in do
        -- Check for duplicate imports.
        let dups = findDuplicates $ map fst nisrcs
        (case takeHead dups of
          Just n -> throw $ ErrorImportDuplicate n
          _      -> return ())

        mapM check nisrcs


---------------------------------------------------------------------------------------------------
-- | Check types of imported capabilities.
checkImportCaps
        :: (Ord n, Show n, Pretty n)
        => Config n -> KindEnv n -> Mode n
        -> [(n, ImportCap n)]
        -> CheckM a n [(n, ImportCap n)]

checkImportCaps config kenv mode nisrcs
 = let
        -- Checker mode to use.
        modeCheckImportCaps
         = case mode of
                Recon   -> Recon
                _       -> Check kEffect

        check (n, isrc)
         = do   let t      =  typeOfImportCap isrc
                (t', k, _) <- checkTypeM config kenv emptyContext UniverseSpec
                                         t modeCheckImportCaps

                -- In Recon mode we need to post-check that the imported
                -- capability really has kind Effect.
                --
                -- In Check mode we pass down the expected kind,
                -- so this is checked locally.
                -- 
                when (not $ isEffectKind k)
                 $ throw $ ErrorImportCapNotEffect n

                return (n, mapTypeOfImportCap (const t') isrc)

    in do
        -- Check for duplicate imports.
        let dups = findDuplicates $ map fst nisrcs
        (case takeHead dups of
          Just n -> throw $ ErrorImportDuplicate n
          _      -> return ())

        mapM check nisrcs


---------------------------------------------------------------------------------------------------
-- | Check types of imported values.
checkImportValues
        :: (Ord n, Show n, Pretty n)
        => Config n -> KindEnv n -> Mode n
        -> [(n, ImportValue n)]
        -> CheckM a n [(n, ImportValue n)]

checkImportValues config kenv mode nisrcs
 = let
        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Check kData

        check (n, isrc)
         = do   let t      =  typeOfImportValue isrc
                (t', k, _) <- checkTypeM config kenv emptyContext UniverseSpec
                                         t modeCheckImportTypes

                -- In Recon mode we need to post-check that the imported
                -- value really has kind Data.
                --
                -- In Check mode we pass down the expected kind,
                -- so this is checked locally.
                --
                when (not $ isDataKind k)
                 $ throw $ ErrorImportValueNotData n

                return  (n, mapTypeOfImportValue (const t') isrc)
   in do
        -- Check for duplicate imports.
        let dups = findDuplicates $ map fst nisrcs
        (case takeHead dups of
          Just n -> throw $ ErrorImportDuplicate n
          _      -> return ())

        mapM check nisrcs


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

