
module DDC.Core.Check.Judge.Module
        ( checkModuleIO, reconModule
        , checkModuleM)
where
import DDC.Core.Check.Context.Oracle
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

import qualified DDC.Core.Env.EnvX              as EnvX
import qualified DDC.Core.Check.Context.Oracle  as Oracle
import qualified DDC.Core.Check.Post            as Post
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
        -> Maybe (Oracle n) -- ^ Checker oracle if we want to allow imports from other modules.
        -> Module a n       -- ^ Module to check.
        -> Mode n           -- ^ Type checker mode.
        -> IO ( Either (Error a n) (Module (AnTEC a n) n)
              , CheckTrace )

checkModuleIO config mOracle xx mode
 = do   ((tr, _, _), result)
         <- runCheck (mempty, 0, 0)
         $  checkModuleM config mOracle xx mode

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
        => Config n          -- ^ Type checker configuration defines what features to support.
        -> Maybe (Oracle n)  -- ^ Checker oracle if we want to allow imports from other modules.
        -> Module a n        -- ^ Module check.
        -> Mode n            -- ^ Type checker mode.
        -> CheckM a n (Module (AnTEC a n) n)

checkModuleM config mOracle0 mm@ModuleCore{} !mode
 = do
        -- Initialize the Environemnt -----------------------------------------
        ctrace  $ vcat
                [ text "* Priming the Oracle"
                , text "  Imported Modules:"
                , indent 4 $ vcat $ map ppr $ moduleImportModules mm
                , mempty ]

        -- Tell the oracle to bring bindings from the imported modules into scope.
        mOracle
         <- case mOracle0 of
                Nothing
                 -> return mOracle0

                Just oracle
                 -> fmap Just $ liftIO
                 $  Oracle.importModules oracle $ moduleImportModules mm

        -- Build the top level kind environment from the module.
        -- We can reuse the same context when checking top level type declarations
        -- because we don't need to allocate any metavariables. However, we do
        -- need the contained oracle as the declarations may mention names of
        -- other things in external modules.
        let ctx0 = (contextOfEnvX
                        (moduleEnvX (configPrimKinds config)
                                    (configPrimTypes config)
                                    (configPrimDataDefs config)
                                    mm))
                 { contextOracle = mOracle }

        -- Check sorts of imported types --------------------------------------
        ctrace  $ vcat
                [ text "* Checking Sorts of Imported Types" ]

        nitsImportType'
                <- checkImportTypes  config ctx0 mode
                $  moduleImportTypes mm

        -- Check imported data types ------------------------------------------
        ctrace  $ vcat
                [ text "* Checking Sorts of Imported Data Types." ]

        _nksImportDataDef'
                <- checkSortsOfDataTypes config ctx0 mode
                $  map snd $ moduleImportDataDefs  mm

        -- Check local data types ---------------------------------------------
        ctrace  $ vcat
                [ text "* Checking Sorts of Local Data Types." ]

        _nksLocalDataDef'
                <- checkSortsOfDataTypes config ctx0 mode
                $  map snd $ moduleLocalDataDefs   mm

        -- Check kinds of imported type equations -----------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Type Equations."]

        nktsImportTypeDef'
                <- checkKindsOfTypeDefs config ctx0
                $  moduleImportTypeDefs mm

        -- Check kinds of local type equations --------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Local Type Equations."]

        nktsLocalTypeDef'
                <- checkKindsOfTypeDefs config ctx0
                $  moduleLocalTypeDefs  mm

        -- Check imported data type defs --------------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Imported Data Types."]

        let dataDefsImported = map snd $ moduleImportDataDefs mm

        -- TODO: data defs checker needs the oracle.
        dataDefsImported'
         <- case checkDataDefs config (contextEnvT ctx0) dataDefsImported of
                (err : _, _)            -> throw $ ErrorData err
                ([], dataDefsImported') -> return dataDefsImported'

        -- Check the local data defs ------------------------------------------
        ctrace  $ vcat
                [ text "* Checking Kinds of Local Data Types."]

        let dataDefsLocal    = map snd $ moduleLocalDataDefs mm

        -- TODO: data defs checker needs the oracle.
        dataDefsLocal'
         <- case checkDataDefs config (contextEnvT ctx0) dataDefsLocal of
                (err : _, _)            -> throw $ ErrorData err
                ([], dataDefsLocal')    -> return dataDefsLocal'

        -- Check types of imported capabilities -------------------------------
        ctrace  $ text "* Checking Kinds of Imported Capabilities."

        ntsImportCap'
                <- checkImportCaps  config ctx0 mode
                $  moduleImportCaps mm

        -- Check types of imported values ------------------------------------
        ctrace  $ text "* Checking Types of Imported Values."

        let envX
                = moduleEnvX (configPrimKinds config)
                             (configPrimTypes config)
                             (configPrimDataDefs config)
                $ mm
                { moduleImportTypes     = nitsImportType'
                , moduleImportTypeDefs  = nktsImportTypeDef'
                , moduleImportDataDefs  = [ (dataDefTypeName def, def) | def <- dataDefsImported']
                , moduleImportCaps      = ntsImportCap'
                , moduleLocalTypeDefs   = nktsLocalTypeDef'
                , moduleLocalDataDefs   = [ (dataDefTypeName def, def) | def <- dataDefsLocal' ] }

        ntsImportValue'
                <- checkImportValues  config ctx0 mode
                $  moduleImportValues mm

        -----------------------------------------------------------------------
        -- Build the top-level config, defs and environments.
        --  These contain names that are visible to bindings in the module.
        let ctx_top
                = emptyContext
                { contextOracle = mOracle
                , contextEnvX   = envX }

        -- Check the sigs of exported types ---------------
        ctrace  $ text "* Checking Kinds of Exported Types"
        esrcsType'  <- checkExportTypes   config ctx0
                    $  moduleExportTypes  mm

        -- Check the sigs of exported values --------------
        ctrace  $ text "* Checking Types of Exported Values"
        esrcsValue' <- checkExportValues  config ctx0
                    $  moduleExportValues mm

        -- Check the body of the module -------------------
        ctrace  $ text "* Checking Module Body"
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
                , moduleImportDataDefs  = [ (dataDefTypeName def, def) | def <- dataDefsImported']
                , moduleImportCaps      = ntsImportCap'
                , moduleImportValues    = ntsImportValue'
                , moduleLocalTypeDefs   = nktsLocalTypeDef'
                , moduleLocalDataDefs   = [ (dataDefTypeName def, def) | def <- dataDefsLocal' ]
                , moduleBody            = xx_annot }

        -- Check that each exported signature matches the type of its binding.
        -- This returns an environment containing all the bindings defined
        -- in the module.
        envX_binds
         <- checkModuleBinds envX
                (moduleExportTypes  mm_inferred)
                (moduleExportValues mm_inferred)
                xx_annot

        -- Check that all exported bindings are defined by the module,
        --   either directly as bindings, or by importing them from somewhere else.
        --   Header modules don't need to contain the complete set of bindings,
        --   but all other modules do.
        when (not $ moduleIsHeader mm_inferred)
                $ mapM_ (checkBindDefined envX_binds mOracle)
                $ map fst $ moduleExportValues mm_inferred

        -- If exported names are missing types then fill them in.
        let updateExportValue (ExportValueLocalNoType n)
                -- Exported thing was foreign imported from Sea land.
                | Just (ImportValueSea mn _ nExternal t) <- lookup n ntsImportValue'
                = return $ ExportValueSea mn n nExternal t

                -- Exported thing was imported from another module.
                | Just (ImportValueModule mn _ t ma)  <- lookup n ntsImportValue'
                = return $ ExportValueLocal mn n t ma

                -- Exported thing was defined in this module.
                | Just t  <- EnvX.lookupX (UName n) envX_binds
                = return $ ExportValueLocal (moduleName mm) n t Nothing

                -- Exported thing was imported via a module import.
                | Just oracle <- mOracle
                = Oracle.resolveValueName oracle n
                >>= \case
                        Just iv -> return $ exportOfImportValue iv
                        Nothing -> throw $ ErrorExportUndefined n

                | otherwise
                = throw $ ErrorExportUndefined n

            updateExportValue e = return e

        let (nsExport, evsExport) = unzip esrcsValue'
        evsExport'      <- mapM updateExportValue evsExport
        let esrcsValue_updated = zip nsExport evsExport'

        -- Return the checked bindings as they have explicit type annotations.
        let mm_updated
                = mm_inferred
                { moduleExportValues    = esrcsValue_updated  }

        -- Use the oracle cache to add import declarations that close the module,
        -- also remove any 'import module' constructs.
        mm_closed
         <- case contextOracle ctx of
                Nothing     -> return mm_updated
                Just oracle
                 -> liftIO $ closeModuleWithOracle (configPrimKinds config) oracle mm_updated

        return mm_closed


---------------------------------------------------------------------------------------------------
-- | Check kinds of data type definitions,
--   returning a map of data type constructor constructor name to its kind.
checkSortsOfDataTypes
        :: (Ord n, Show n, Pretty n)
        => Config n
        -> Context n
        -> Mode n
        -> [DataDef n]
        -> CheckM a n [(n, Kind n)]

checkSortsOfDataTypes config ctx mode defs
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
                   $ checkTypeM config ctx UniverseKind k modeCheckDataTypes
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
        -> Context n
        -> [(n, (Kind n, Type n))]
        -> CheckM a n [(n, (Kind n, Type n))]

checkKindsOfTypeDefs config ctx nkts
 = let
        -- Check a single type equation.
        check (n, (_k, t))
         = do   (t', k', _)
                 <- checkTypeM config ctx UniverseSpec t Recon

                -- ISSUE #374: Check specified kinds of type equations against inferred kinds.
                -- The Source -> Core transform fills in the kind with a hole, but we should
                -- allow the result kind to be specified in the source language to be consistent
                -- with value bindings.
                --
                -- The fact that we rely on kind reconstruction here also means that type
                -- decls that mention each other must appear in the module in use-def order,
                -- otherwise the checker will complain about the ? (hole) kind.

                return (n, (k', t'))

   in do
        -- ISSUE #373: Check that type equations are not recursive.
        nkts' <- mapM check nkts
        return nkts'

