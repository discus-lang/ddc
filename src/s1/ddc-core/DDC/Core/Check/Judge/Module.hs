
module DDC.Core.Check.Judge.Module
        ( checkModuleIO, reconModule
        , checkModuleM)
where
import DDC.Core.Check.Judge.Module.Binds
import DDC.Core.Check.Judge.Module.Exports
import DDC.Core.Check.Judge.Module.Imports
import DDC.Core.Check.Judge.Type.Base           (checkTypeM)
import DDC.Core.Check.Judge.DataDefs
import DDC.Core.Check.Close
import DDC.Core.Check.Base
import DDC.Core.Check.Exp
import DDC.Core.Interface.Store
import DDC.Core.Transform.Reannotate
import DDC.Core.Transform.MapT
import DDC.Core.Module
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


        -- Check imported data type defs --------------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Data Types."]

        let dataDefsImported = map snd $ moduleImportDataDefs mm
        dataDefsImported'
         <- case checkDataDefs config envT_importedTypeDefs dataDefsImported of
                (err : _, _)            -> throw $ ErrorData err
                ([], dataDefsImported') -> return dataDefsImported'

        -- TODO: this is dodgy.
        -- The data defs / synonyms / foreign type should be able to be recursive.
        let envT_importedDataDefs
                = EnvT.unions
                [ envT_importedTypeDefs
                , EnvT.fromListNT [ (dataDefTypeName def, kindOfDataDef def)
                                  | def <- dataDefsImported' ] ]

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
                        envT_importedDataDefs
                         { EnvT.envtEquations
                         = Map.map snd $ Map.fromList $ moduleLocalTypeDefs mm }
                $  moduleLocalTypeDefs  mm

        let envT_localTypeDefs
                = EnvT.unions
                [ envT_importedDataDefs
                , EnvT.fromListNT [ (n, k) | (n, (k, _)) <- nktsLocalTypeDef']
                , EnvT.empty
                        { EnvT.envtEquations
                        = Map.unions
                                [ Map.fromList [ (n, t) | (n, (_, t)) <- nktsLocalTypeDef']
                                , Map.fromList [ (n, t) | (n, (_, t)) <- nktsImportTypeDef' ]]
                        }
                ]

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

        let envX_importCaps
                = EnvX.empty
                { EnvX.envxEnvT         = envT_importCaps
                , EnvX.envxDataDefs     = dataDefs_top }

        -- Check types of imported values ------------------------------------
        ctrace  $ vcat
                [ text "* Checking Types of Imported Values."
                , mempty ]

--        ctrace  $ string $ show (Map.keys $ EnvT.envtEquations envT_importCaps)
        ctrace  $ string $ show nktsImportTypeDef'

        ntsImportValue'
                <- checkImportValues  config envX_importCaps mode
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

checkKindsOfTypeDefs _config env nkts
 = let
        -- TODO: we're not checking type synonyms at all.
        _ctx     = contextOfEnvT env

        -- Check a single type equation.
        check (n, (k, t))
         = do   -- (t', k', _)
                -- <- checkTypeM config ctx UniverseSpec t Recon

                -- ISSUE #374: Check specified kinds of type equations against inferred kinds.
                return (n, (k, t))

   in do
        -- ISSUE #373: Check that type equations are not recursive.
        nkts' <- mapM check nkts
        return nkts'


