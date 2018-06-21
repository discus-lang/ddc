
-- | Add code to initialize the module.
module DDC.Core.Discus.Transform.Initialize
        (initializeModule)
where
import qualified DDC.Type.Exp.Simple            as C
import qualified DDC.Type.DataDef               as C
import qualified DDC.Core.Exp.Annot             as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Discus                as D
import qualified DDC.Core.Discus.Compounds      as D
import qualified Data.Text                      as T
import Data.List


---------------------------------------------------------------------------------------------------
-- | Insert inititialization code into the module.
initializeModule
        :: A.Config             -- ^ Runtime system configuration.
        -> [C.ModuleName]       -- ^ Names of modules transitively imported by this one.
        -> C.Module a D.Name
        -> C.Module a D.Name

initializeModule config mnsImport mm
 | C.isMainModuleName $ C.moduleName mm
 = initializeMain config
 $ initializeInfo mnsImport mm

 | otherwise
 = initializeInfo [] mm


---------------------------------------------------------------------------------------------------
-- | Insert initialization code into a module.
initializeInfo
        :: [C.ModuleName]       -- ^ Also call infotable init functions for these modules.
        -> C.Module a D.Name    -- ^ Module to add initialization code to.
        -> C.Module a D.Name

initializeInfo mnsImport mm
 = let  modName  = C.moduleName mm
        dataDefs = map snd $ C.moduleLocalDataDefs mm

        importValueSea n t
         = (D.NameVar n, C.ImportValueSea (D.NameVar n) n t)

   in mm
        { C.moduleBody
            = injectInfoInit modName mnsImport dataDefs
            $ C.moduleBody   mm

        , C.moduleExportValues
            =  C.moduleExportValues mm
            ++ [ ( initNameOfModule modName
                 , C.ExportValueLocal
                        { exportValueLocalModuleName    = modName
                        , exportValueLocalName          = initNameOfModule modName
                        , exportValueLocalType          = D.tUnit `D.tFun` D.tUnit
                        , exportValueLocalArity         = Just (0, 1, 0) }) ]

             -- TODO: these will need to be converted into primops so that the discus -> salt
             -- code generator can remember the info table index of each data constructor.
        , C.moduleImportValues
            =  C.moduleImportValues mm
            ++ [ importValueSea "ddcInfoFrameNew"     D.tInfoFrameNew
               , importValueSea "ddcInfoFramePush"    D.tInfoFramePush
               , importValueSea "ddcInfoFrameAddData" D.tInfoFrameAddData ]

               -- Import the initialization functions for transitively imported modules.
            ++ [ ( initNameOfModule mn
                 , C.ImportValueModule
                        { importValueModuleName  = mn
                        , importValueModuleVar   = initNameOfModule mn
                        , importValueModuleType  = D.tUnit `D.tFun` D.tUnit
                        , importValueModuleArity = Just (0, 1, 0) })
               | mn <- mnsImport ]
        }


-- | Get the name of the initialization function for the given module name.
initNameOfModule :: C.ModuleName -> D.Name
initNameOfModule (C.ModuleName parts)
 = let  flatName = intercalate "$" parts
   in   D.NameVar $ T.pack $ "_init$" ++ flatName


-- | Get the binding ccc of the initialization function for the given module name.
initBindOfModule :: C.ModuleName -> C.Bind D.Name
initBindOfModule mn  = C.BName (initNameOfModule mn) $ D.tUnit `D.tFun` D.tUnit


-- | Get the bound occ of the initialization function for the given module name.
initBoundOfModule :: C.ModuleName -> C.Bound D.Name
initBoundOfModule mn = C.UName $ initNameOfModule mn


-- | Inject the info table initialization function into the end of the module.
injectInfoInit
        :: C.ModuleName         -- ^ Name of the module we're making the table for.
        -> [C.ModuleName]       -- ^ Also call the init function for these other modules.
        -> [C.DataDef D.Name]   -- ^ Data type declarations defined locally in the module.
        -> C.Exp a D.Name       -- ^ Body expression of the module to inject into.
        -> C.Exp a D.Name

injectInfoInit modName mnsImport dataDefs xx
 = downTop xx
 where
        downTop x
         = case x of
                C.XLLet a b x1 x2 -> C.XLLet a b x1 (downTop x2)
                C.XLRec a bxs x2  -> C.XLRec a (bxs ++ [(b', xInit')]) x2
                _                 -> x

        a'      = D.annotOfExp xx
        b'      = initBindOfModule modName

        xInit'  = C.XAbs a' (C.MTerm (C.BNone D.tUnit))
                $ makeInfoInitImport      a' mnsImport
                $ makeInfoInitForDataDefs a' modName dataDefs


-- | Call the info table initialization functions for transitively imported modules.
makeInfoInitImport
        :: a -> [C.ModuleName]
        -> C.Exp a D.Name -> C.Exp a D.Name

makeInfoInitImport a mnsImport xx
 = foldr (C.XLet a) xx
 $ map makeForModuleName mnsImport

 where  makeForModuleName mn
         = let  u'       = initBoundOfModule mn
           in   C.LLet (C.BNone D.tUnit) (C.XApp a (C.XVar a u') (C.RTerm (C.xUnit a)))


-- | Create a new frame to hold info about the given data type definitions,
--   and push it on the frame stack.
makeInfoInitForDataDefs
        :: a
        -> C.ModuleName         -- ^ Name of module the data types are defined in.
        -> [C.DataDef D.Name]   -- ^ Data type definitions to create info table frames for.
        -> C.Exp a D.Name

makeInfoInitForDataDefs a modName dataDefs
 = foldr (C.XLet a) (D.xUnit a)
 $ concatMap (makeInfoInitForDataDef a modName) dataDefs


-- | Create a new frame to hold info about the given data type definition.
makeInfoInitForDataDef
        :: a
        -> C.ModuleName         -- ^ Name of the module the data type is define in.
        -> C.DataDef D.Name     -- ^ Data definition to create an info table frame for.
        -> [C.Lets a D.Name]

makeInfoInitForDataDef a modName dataDef
 = case C.dataDefCtors dataDef of
        Nothing    -> []
        Just ctors -> makeFrame ctors
 where
        textLitOfConName nn
         = case nn of
                D.NameCon tx -> D.NameLitTextLit tx
                _ -> error "ddc-core-discus.makeInfoInitForDataDef: invalid ctor name"

        C.ModuleName parts = modName
        flatName = intercalate "." parts

        makeFrame ctors
         =  [ C.LLet (C.BAnon D.tAddr)
                $ D.xInfoFrameNew a (length ctors) ]

         ++ [ C.LLet (C.BNone (D.tWord 32))
                $  D.xInfoFrameAddData a
                        (C.XVar a (C.UIx 0))
                        iTag (length $ C.dataCtorFieldTypes ctor)
                        (C.XCon a (C.DaConPrim (D.NameLitTextLit (T.pack flatName))
                                               (D.tTextLit)))
                        (C.XCon a (C.DaConPrim (textLitOfConName $ C.dataCtorName ctor)
                                               (D.tTextLit)))
                | ctor  <- ctors
                | iTag  <- [0..]]

         ++ [ C.LLet (C.BNone D.tUnit)
                $ D.xInfoFramePush a (C.XVar a (C.UIx 0)) ]


---------------------------------------------------------------------------------------------------
-- | Insert initialization code into the main module.
--
--   The initialization code wraps the main function with the default
--   exception handler.
--
initializeMain :: A.Config -> C.Module a D.Name -> C.Module a D.Name
initializeMain
 (A.Config { A.configHookHandleTopLevel
            = Just (txEffect, txHookHandleTopLevel)}) mm
 | elem (D.NameCon txEffect) $ map fst $ C.moduleImportTypes mm
 = mm   { C.moduleImportValues  = C.moduleImportValues mm ++ [ivHookHandleTopLevel]
        , C.moduleBody          = initializeMainExp nHookHandleTopLevel $ C.moduleBody mm }
 where
        -- ImportValue for the handler.
        ivHookHandleTopLevel
         = ( nHookHandleTopLevel
           , C.ImportValueSea
                { C.importValueSeaNameInternal  = nHookHandleTopLevel
                , C.importValueSeaNameExternal  = txHookHandleTopLevel
                , C.importValueSeaType          = tHookHandleTopLevel })

        nHookHandleTopLevel
         = D.NameVar txHookHandleTopLevel

        tHookHandleTopLevel
         =  C.tForalls [C.kEffect] $ \[tEff]
         -> D.tSusp tEff D.tUnit `D.tFun` D.tSusp (C.tSum C.kEffect [tEff]) D.tUnit

initializeMain _ mm
 = mm


-- | Insert initialization code into the main function of the main module.
initializeMainExp :: D.Name -> C.Exp a D.Name -> C.Exp a D.Name
initializeMainExp nHookHandleTopLevel xx
 = downTop xx
 where
        -- Decend into the module body looking for the 'main' function.
        downTop x
         = case x of
                C.XLLet a b x1 x2 -> C.XLLet a b x1 (downTop x2)
                C.XLRec a bxs x2  -> C.XLRec a (map downBind bxs) (downTop x2)
                _                 -> x

        -- Insert the handler into the main binding.

        -- TODO: We do an inner box run to preserve the arity of the main function,
        --       so that the runtime system can call it directly.
        --       Make sure the main function can be bound directly to a closure value
        --       rather than being a super with this specific arity.
        --
        downBind (C.BName n@(D.NameVar "main") tMain, x)
         | Just (_tParam, tSusp)              <- C.takeTFun tMain
         , Just (tEff,   _tResult)            <- C.takeTSusp tSusp
         , C.XAbs a p@(C.MTerm _bParam) xBody <- x
         = ( C.BName n tMain
           , C.XAbs a p
              $ C.XBox a
              $ C.XLet a (C.LLet  (C.BNone D.tUnit)
                                  (C.xApps a
                                        (C.XVar a (C.UName (D.NameVar "_init$Main")))
                                        [C.RTerm $ D.xUnit a]))
              $ C.XRun a
              $ C.xApps a (C.xApps a (C.XVar a (C.UName nHookHandleTopLevel)) [C.RType tEff])
                          [C.RTerm xBody])

         | otherwise
         = error "ddc-core-discus.initializeModule: invalid type for main binding"

        downBind (b, x)
         = (b, x)

