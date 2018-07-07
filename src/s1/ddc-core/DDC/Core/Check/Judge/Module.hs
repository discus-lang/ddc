
module DDC.Core.Check.Judge.Module
        ( checkModuleIO, reconModule
        , checkModuleM)
where
import DDC.Core.Check.Judge.Type.Base           (checkTypeM)
import DDC.Core.Check.Judge.DataDefs
import DDC.Core.Check.Close
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Interface.Store
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.MapT
import DDC.Core.Module
import DDC.Core.Env.EnvX                        (EnvX)
import DDC.Control.CheckIO                      (runCheck, throw, mapErr)

import qualified DDC.Type.Env                   as Env
import qualified DDC.Core.Env.EnvT              as EnvT
import qualified DDC.Core.Env.EnvX              as EnvX
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Check.Post            as Post
import qualified Data.Map.Strict                as Map
import qualified System.IO.Unsafe               as S


-- Wrappers ---------------------------------------------------------------------------------------
-- | Type check a module.
--
--   If it's good, you get a new version with types attached to all the bound
--   variables
--
--   If it's bad, you get a description of the error.
--
checkModuleIO
        :: (Show a, Ord n, Show n, Pretty n)
        => Config n         -- ^ Type checker configuration defines what features to support.
        -> Maybe (Store n)  -- ^ Interface store if we want to allow imports from other modules.
        -> Module a n       -- ^ Module to check.
        -> Mode n           -- ^ Type checker mode.
        -> IO ( Either (Error a n) (Module (AnTEC a n) n)
              , CheckTrace )

checkModuleIO config mStore xx mode
 = do   ((tr, _, _), result)
         <- runCheck (mempty, 0, 0)
         $  checkModuleM config mStore xx mode

        return (result, tr)


-- | Reconstruct type annotations in a module.
---
--   As Recon mode does not load more interface files it's safe to
--   unsafePerformIO this.
--
reconModule
        :: (Show a, Ord n, Show n, Pretty n)
        => Config n         -- ^ Type checker configuration defines what features to support.
        -> Module a n       -- ^ Module to check.
        -> ( Either (Error a n) (Module (AnTEC a n) n)
           , CheckTrace )

reconModule config xx
        = S.unsafePerformIO
        $ checkModuleIO config Nothing xx Recon


-- checkModule ------------------------------------------------------------------------------------
-- | Like `checkModule` but using the `CheckM` monad to handle errors.
checkModuleM
        :: (Show a, Ord n, Show n, Pretty n)
        => Config n         -- ^ Type checker configuration defines what features to support.
        -> Maybe (Store n)  -- ^ Interface store if we want to allow imports from other modules.
        -> Module a n       -- ^ Module check.
        -> Mode n           -- ^ Type checker mode.
        -> CheckM a n (Module (AnTEC a n) n)

checkModuleM config mStore mm@ModuleCore{} !mode
 = do
        -- Initialize the Environemnt -----------------------------------------
        ctrace  $ vcat
                [ text "* Priming the Oracle"
                , text "  Imported Modules:"
                , indent 4 $ vcat $ map ppr $ moduleImportModules mm
                , mempty ]

        -- Wrap the store into a new interface oracle.
        mOracle_init
         <- case mStore of
                Nothing    -> return $ Nothing
                Just store -> fmap Just $ liftIO $ Oracle.newOracleOfStore store

        -- Tell the oracle to bring bindings from the imported modules into scope.
        mOracle
         <- case mOracle_init of
                Nothing     -> return mOracle_init
                Just oracle
                 -> fmap Just $ liftIO
                 $  Oracle.importModules oracle $ moduleImportModules mm

        -- Build the primitive environment.
        let envT_prim
                = EnvT.empty
                { EnvT.envtPrimFun
                        = \n -> Env.lookupName n (configPrimKinds config) }

        -- Check sorts of imported types --------------------------------------
        ctrace  $ vcat
                [ text "* Checking Sorts of Imported Types" ]

        --   These have explicit kind annotations on the type parameters,
        --   which we can sort check directly.
        nitsImportType'
                <- checkImportTypes  config envT_prim mode
                $  moduleImportTypes mm

        let nksImportType'
                = [(n, kindOfImportType i) | (n, i) <- nitsImportType']

        let envT_import
                = EnvT.empty
                { EnvT.envtForeignTypes
                        = Map.fromList $ nitsImportType' }

        -- Check sorts of imported data types ---------------------------------
        ctrace  $ vcat
                [ text "* Checking Sorts of Imported Data Types." ]

        --   These have explicit kind annotations on the type parameters,
        --   which we can sort check directly.
        nksImportDataDef'
                <- checkSortsOfDataTypes config mode
                $  map snd $ moduleImportDataDefs  mm

        ctrace  $ vcat
                [ text "* Checking Sorts of Local Data Types." ]

        nksLocalDataDef'
                <- checkSortsOfDataTypes config mode
                $  map snd $ moduleLocalDataDefs   mm

        let envT_dataDefs
                = EnvT.unions
                [ envT_prim
                , envT_import
                , EnvT.fromListNT nksImportType'
                , EnvT.fromListNT nksImportDataDef'
                , EnvT.fromListNT nksLocalDataDef' ]

        -- Check kinds of imported type equations -----------------------------
        --   The right of each type equation can mention both imported abstract
        --   types and data type definitions, so we need to include them in
        --   the kind environment as well.
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Type Equations."]

        -- Imported type equations may mention each other.
        nktsImportTypeDef'
                <- checkKindsOfTypeDefs
                        config
                        envT_dataDefs
                          { EnvT.envtEquations
                          = Map.map snd $ Map.fromList $ moduleImportTypeDefs mm }
                $  moduleImportTypeDefs mm

        let envT_importedTypeDefs
                = EnvT.unions
                [ envT_dataDefs
                , EnvT.fromListNT [ (n, k) | (n, (k, _)) <- nktsImportTypeDef']
                , EnvT.empty
                        { EnvT.envtEquations
                        = Map.fromList    [ (n, t) | (n, (_, t)) <- nktsImportTypeDef']}]

        -- Check kinds of local type equations --------------------------------
        --   The right of each type equation can mention
        --   imported abstract types, imported and local data type definitions.
        ctrace  $ vcat
                [ text "* Checking Kinds of Local Type Equations."]

        -- Kinds of type constructors in scope in the
        -- locally defined type equations.
        nktsLocalTypeDef'
                <- checkKindsOfTypeDefs
                        config
                        envT_importedTypeDefs
                         { EnvT.envtEquations
                         = Map.map snd $ Map.fromList $ moduleLocalTypeDefs mm }
                $  moduleLocalTypeDefs  mm

        let envT_localTypeDefs
                = EnvT.unions
                [ envT_importedTypeDefs
                , EnvT.fromListNT [ (n, k) | (n, (k, _)) <- nktsLocalTypeDef']
                , EnvT.empty
                        { EnvT.envtEquations
                        = Map.unions
                                [ Map.fromList [ (n, t) | (n, (_, t)) <- nktsLocalTypeDef']
                                , Map.fromList [ (n, t) | (n, (_, t)) <- nktsImportTypeDef' ]]
                        }
                ]

        -- Check imported data type defs --------------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Data Types."]

        let dataDefsImported = map snd $ moduleImportDataDefs mm
        dataDefsImported'
         <- case checkDataDefs config envT_localTypeDefs dataDefsImported of
                (err : _, _)            -> throw $ ErrorData err
                ([], dataDefsImported') -> return dataDefsImported'

        -- Check the local data defs ------------------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Local Data Types."]

        let dataDefsLocal    = map snd $ moduleLocalDataDefs mm
        dataDefsLocal'
         <- case checkDataDefs config envT_localTypeDefs dataDefsLocal of
                (err : _, _)            -> throw $ ErrorData err
                ([], dataDefsLocal')    -> return dataDefsLocal'

        let dataDefs_top
                = unionDataDefs (configPrimDataDefs config)
                $ unionDataDefs (fromListDataDefs dataDefsImported')
                                (fromListDataDefs dataDefsLocal')

        -- Check types of imported capabilities -------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Capabilities."]

        ntsImportCap'
                <- checkImportCaps  config envT_localTypeDefs mode
                $  moduleImportCaps mm

        let envT_importCaps
                = EnvT.unions
                [ envT_localTypeDefs
                , EnvT.empty
                        { EnvT.envtCapabilities
                           = Map.fromList
                           $ [ (n, t) | (n, ImportCapAbstract t) <- ntsImportCap'] }]

        -- Check types of imported values ------------------------------------
        ctrace  $ vcat
                [ text "* Checking Types of Imported Values."
                , mempty ]

        ntsImportValue'
                <- checkImportValues  config envT_importCaps mode
                $  moduleImportValues mm

        let envX_importValues
                = (EnvX.fromListNT [(n, typeOfImportValue i) | (n, i) <- ntsImportValue' ])
                {  EnvX.envxEnvT     = envT_importCaps
                ,  EnvX.envxDataDefs = dataDefs_top
                ,  EnvX.envxPrimFun  = \n -> Env.envPrimFun (configPrimTypes config) n }

        -----------------------------------------------------------------------
        -- Build the top-level config, defs and environments.
        --  These contain names that are visible to bindings in the module.
        let envT_top    = envT_importCaps
        let envX_top    = envX_importValues
        let ctx_top     = emptyContext
                        { contextOracle = mOracle
                        , contextEnvX   = envX_top }

        -- Check the sigs of exported types ---------------
        esrcsType'  <- checkExportTypes   config envT_top
                    $  moduleExportTypes  mm

        -- Check the sigs of exported values --------------
        esrcsValue' <- checkExportValues  config envX_top
                    $  moduleExportValues mm


        -- Check the body of the module -------------------
        (x', _, _effs, ctx)
         <- checkExpM   (makeTable config)
                        ctx_top mode DemandNone (moduleBody mm)

        -- Apply the final context to the annotations in expressions.
        let applyToAnnot (AnTEC t0 e0 _ x0)
             = do t0' <- applySolved ctx t0
                  e0' <- applySolved ctx e0
                  return $ AnTEC t0' e0' (tBot kClosure) x0

        xx_solved <- mapT (applySolved ctx) x'
        xx_annot  <- reannotateM applyToAnnot xx_solved

        -- Post check the annotate expression to ensure there are no unsolved metavariables.
        (case Post.checkExp xx_annot of
                Left (ErrorAmbiguousType a)
                  -> throw $ ErrorAmbiguousType $ annotTail a
                Left (ErrorAmbiguousTypeExp a x)
                  -> throw $ ErrorAmbiguousTypeExp (annotTail a) (reannotate annotTail x)
                _ -> return ())

        -- Build new module with infered annotations ------
        let mm_inferred
                = mm
                { moduleExportTypes     = esrcsType'
                , moduleImportTypes     = nitsImportType'
                , moduleImportTypeDefs  = nktsImportTypeDef'
                , moduleImportCaps      = ntsImportCap'
                , moduleImportValues    = ntsImportValue'
                , moduleLocalTypeDefs   = nktsLocalTypeDef'
                , moduleBody            = xx_annot }


        -- Check that each exported signature matches the type of its binding.
        -- This returns an environment containing all the bindings defined
        -- in the module.
        envX_binds
         <- checkModuleBinds envX_top
                (moduleExportTypes  mm_inferred)
                (moduleExportValues mm_inferred)
                xx_annot

        -- Check that all exported bindings are defined by the module,
        --   either directly as bindings, or by importing them from somewhere else.
        --   Header modules don't need to contain the complete set of bindings,
        --   but all other modules do.
        when (not $ moduleIsHeader mm_inferred)
                $ mapM_ (checkBindDefined envX_binds)
                $ map fst $ moduleExportValues mm_inferred

        -- If exported names are missing types then fill them in.
        let updateExportValue e
                -- Exported thing was foreign imported from Sea land.
                | ExportValueLocalNoType n  <- e
                , Just (ImportValueSea mn _ nExternal t) <- lookup n ntsImportValue'
                = ExportValueSea mn n nExternal t

                -- Exported thing was imported from another module.
                | ExportValueLocalNoType n <- e
                , Just (ImportValueModule mn _ t ma)  <- lookup n ntsImportValue'
                = ExportValueLocal mn n t ma

                -- Exported thing was defined in this module.
                | ExportValueLocalNoType n  <- e
                , Just t  <- EnvX.lookupX (UName n) envX_binds
                = ExportValueLocal (moduleName mm) n t Nothing

                | otherwise = e

        let esrcsValue_updated
                = [ (n, updateExportValue e) | (n, e) <- esrcsValue' ]

        -- Return the checked bindings as they have explicit type annotations.
        let mm_updated
                = mm_inferred
                { moduleExportValues    = esrcsValue_updated  }

        -- Use the oracle cache to add import declarations that close the module,
        -- also remove any 'import module' constructs.
        mm_closed
         <- case contextOracle ctx of
                Nothing     -> return mm_updated
                Just oracle -> closeModuleWithOracle oracle mm_updated

        return mm_closed


---------------------------------------------------------------------------------------------------
-- | Check exported types.
checkExportTypes
        :: (Show n, Pretty n, Ord n)
        => Config  n
        -> EnvT  n
        -> [(n, ExportType n (Type n))]
        -> CheckM a n [(n, ExportType n (Type n))]

checkExportTypes config env nesrcs
 = let
        ctx     = contextOfEnvT env

        check (n, esrc)
         | Just k          <- takeKindOfExportType esrc
         = do   (k', _, _) <- checkTypeM config ctx UniverseKind k Recon
                return  $ (n, mapKindOfExportType (const k') esrc)

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
        => Config n
        -> EnvX   n
        -> [(n, ExportValue n (Type n))]
        -> CheckM a n [(n, ExportValue n (Type n))]

checkExportValues config envX nesrcs
 = let
        ctx     = contextOfEnvX envX

        check (n, esrc)
         | Just t          <- takeTypeOfExportValue esrc
         = do   (t', _, _) <- checkTypeM config ctx UniverseSpec t Recon
                return  $ (n, mapTypeOfExportValue (const t') esrc)

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
        => Config n
        -> EnvT   n
        -> Mode   n
        -> [(n, ImportType n (Type n))]
        -> CheckM a n [(n, ImportType n (Type n))]

checkImportTypes config env mode nisrcs
 = let
        ctx     = contextOfEnvT env

        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Synth []

        -- Check an import definition.
        check (n, isrc)
         = do   let k      =  kindOfImportType isrc
                (k', _, _) <- checkTypeM config ctx UniverseKind k modeCheckImportTypes
                return  (n, mapKindOfImportType (const k') isrc)

        -- Pack down duplicate import definitions.
        --   We can import the same value via multiple modules,
        --   which is ok provided all instances have the same kind.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                 | compat isrc isrc' -> pack mm nis
                 | otherwise         -> throw $ ErrorImportDuplicate n

                Nothing              -> pack (Map.insert n isrc mm) nis

        -- Check if two import definitions with the same name are compatible.
        -- The same import definition can appear multiple times provided
        -- each instance has the same name and kind.
        compat (ImportTypeAbstract k1) (ImportTypeAbstract k2)
                = equivT env k1 k2

        compat (ImportTypeBoxed    k1) (ImportTypeBoxed    k2)
                = equivT env k1 k2

        compat _ _ = False

   in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that exports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


-------------------------------------------------------------------------------
-- | Check kinds of data type definitions,
--   returning a map of data type constructor constructor name to its kind.
checkSortsOfDataTypes
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> Mode n
        -> [DataDef n]
        -> CheckM a n [(n, Kind n)]

checkSortsOfDataTypes config mode defs
 = let
        -- Checker mode to use.
        modeCheckDataTypes
         = case mode of
                Recon   -> Recon
                _       -> Synth []

        -- Check kind of a data type constructor.
        check def
         = do   let k   = kindOfDataDef def
                (k', _, _)
                  <- mapErr (ErrorCtxData (dataDefTypeName def))
                   $ checkTypeM config emptyContext UniverseKind k modeCheckDataTypes
                return (dataDefTypeName def, k')

   in do
        -- Check all the imports individually.
        nks     <- mapM check defs
        return  nks


---------------------------------------------------------------------------------------------------
-- | Check kinds of imported type equations.
checkKindsOfTypeDefs
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT n
        -> [(n, (Kind n, Type n))]
        -> CheckM a n [(n, (Kind n, Type n))]

checkKindsOfTypeDefs config env nkts
 = let
        ctx     = contextOfEnvT env

        -- Check a single type equation.
        check (n, (_k, t))
         = do   (t', k', _)
                 <- checkTypeM config ctx UniverseSpec t Recon

                -- ISSUE #374: Check specified kinds of type equations against inferred kinds.
                return (n, (k', t'))

   in do
        -- ISSUE #373: Check that type equations are not recursive.
        nkts' <- mapM check nkts
        return nkts'


---------------------------------------------------------------------------------------------------
-- | Check types of imported capabilities.
checkImportCaps
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT n
        -> Mode n
        -> [(n, ImportCap n (Type n))]
        -> CheckM a n [(n, ImportCap n (Type n))]

checkImportCaps config env mode nisrcs
 = let
        ctx     = contextOfEnvT env

        -- Checker mode to use.
        modeCheckImportCaps
         = case mode of
                Recon   -> Recon
                _       -> Check kEffect

        -- Check an import definition.
        check (n, isrc)
         = do   let t      =  typeOfImportCap isrc
                (t', k, _) <- checkTypeM config ctx UniverseSpec
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

        -- Pack down duplicate import definitions.
        --   We can import the same capability via multiple modules,
        --   which is ok provided all instances have the same type.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                 | compat isrc isrc'    -> pack mm nis
                 | otherwise            -> throw $ ErrorImportDuplicate n

                Nothing                 -> pack (Map.insert n isrc mm) nis

        -- Check if two imported capabilities of the same name are compatiable.
        -- The same import definition can appear multiple times provided each
        -- instance has the same name and type.
        compat (ImportCapAbstract t1) (ImportCapAbstract t2)
                = equivT (contextEnvT ctx) t1 t2

    in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that imports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


---------------------------------------------------------------------------------------------------
-- | Check types of imported values.
checkImportValues
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> EnvT n
        -> Mode n
        -> [(n, ImportValue n (Type n))]
        -> CheckM a n [(n, ImportValue n (Type n))]

checkImportValues config env mode nisrcs
 = let
        ctx = contextOfEnvT env

        -- Checker mode to use.
        modeCheckImportTypes
         = case mode of
                Recon   -> Recon
                _       -> Check kData

        -- Check an import definition.
        check (n, isrc)
         = do   let t      =  typeOfImportValue isrc
                (t', k, _) <- checkTypeM config ctx UniverseSpec
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

        -- Pack down duplicate import definitions.
        --   We can import the same value via multiple modules,
        --   which is ok provided all instances have the same type.
        pack !mm []
         = return $ Map.toList mm

        pack !mm ((n, isrc) : nis)
         = case Map.lookup n mm of
                Just isrc'
                  | compat isrc isrc'   -> pack mm nis
                  | otherwise           -> throw $ ErrorImportDuplicate n

                Nothing                 -> pack (Map.insert n isrc mm) nis

        -- Check if two imported values of the same name are compatable.
        compat (ImportValueModule _ _ t1 a1)
               (ImportValueModule _ _ t2 a2)
         = equivT (contextEnvT ctx) t1 t2 && a1 == a2

        compat (ImportValueSea _ _ _ t1)
               (ImportValueSea _ _ _ t2)
         = equivT (contextEnvT ctx) t1 t2

        compat _ _ = False

   in do
        -- Check all the imports individually.
        nisrcs' <- mapM check nisrcs

        -- Check that imports with the same name are compatable,
        -- and pack down duplicates.
        pack Map.empty nisrcs'


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
        :: Ord n
        => EnvX n               -- ^ Environment containing binds defined by the module.
        -> n                    -- ^ Name of an exported binding.
        -> CheckM a n ()

checkBindDefined envx n
 = case EnvX.lookupX (UName n) envx of
        Just _  -> return ()
        _       -> throw $ ErrorExportUndefined n

