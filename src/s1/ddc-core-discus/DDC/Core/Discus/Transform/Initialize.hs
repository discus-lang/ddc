{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- | Add code to initialize the module.
module DDC.Core.Discus.Transform.Initialize
        (initializeModule)
where
import qualified DDC.Core.Discus                        as D
import qualified DDC.Core.Discus.Compounds              as D
import qualified DDC.Core.Salt.Runtime                  as A
import qualified DDC.Type.Exp.Simple                    as C
import qualified DDC.Type.DataDef                       as C
import qualified DDC.Core.Exp.Annot                     as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Codec.Shimmer.Hash            as C
import qualified DDC.Core.Codec.Shimmer.Encode          as C.Encode
import qualified DDC.Core.Discus.Codec.Shimmer.Encode   as D.Encode
import qualified DDC.Core.Call                          as C
import qualified Data.Text                              as T
import qualified Data.Set                               as Set
import Data.Monoid
import Data.List
import Data.Maybe


---------------------------------------------------------------------------------------------------
-- | Insert inititialization code into the module.
initializeModule
        :: A.Config             -- ^ Runtime system configuration.
        -> C.Module a D.Name
        -> C.Module a D.Name

initializeModule config mm
 | C.isMainModuleName $ C.moduleName mm
 = initializeInfo $ initializeMain config mm

 | otherwise
 = initializeInfo mm


---------------------------------------------------------------------------------------------------
-- | Insert initialization code into a module.
initializeInfo :: C.Module a D.Name -> C.Module a D.Name
initializeInfo mm
 = let  modName   = C.moduleName mm

        mnsImport = if C.isMainModuleName $ C.moduleName mm
                        then Set.toList $ C.moduleTransitiveDeps mm
                        else []

        dataDefs  = map snd $ C.moduleLocalDataDefs mm
        superDefs = slurpSuperDefs modName $ C.moduleBody mm

   in mm
        { C.moduleBody
            = injectInfoInit modName mnsImport dataDefs superDefs
            $ C.moduleBody   mm

        , C.moduleExportValues
            =  C.moduleExportValues mm
            ++ [ ( initNameOfModule modName
                 , C.ExportValueLocal
                        { exportValueLocalModuleName    = modName
                        , exportValueLocalName          = initNameOfModule modName
                        , exportValueLocalType          = D.tUnit `D.tFun` D.tUnit
                        , exportValueLocalArity         = Just (0, 1, 0) }) ]

        , C.moduleImportValues
            =  C.moduleImportValues mm

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


---------------------------------------------------------------------------------------------------
data SuperDef a
        = SuperDef
        { superDefModuleName    :: C.ModuleName
        , superDefName          :: T.Text
        , superDefParams        :: Int
        , superDefBoxes         :: Int
        , superDefExp           :: C.Exp a D.Name }
        deriving Show


-- | Slurp super definitions from the top-level of the module.
slurpSuperDefs
        :: C.ModuleName
        -> C.Exp a D.Name
        -> [SuperDef a]

slurpSuperDefs _modName xx
 = downTop xx
 where
        downTop x
         = case x of
                C.XLLet _a b x1 x2 -> maybeToList (slurpSuperDef b x1) ++ downTop x2
                C.XLRec _a bxs x2  -> mapMaybe (uncurry slurpSuperDef) bxs ++ downTop x2
                _                  -> []

        -- Some top level thing that looks like a super we can
        -- make an info table entry for.
        slurpSuperDef (D.BName n _t) x
         | Just (_csType, csValue, csBox)
                <-  C.splitStdCallCons
                $   C.takeCallConsFromExp x
         = let  txName = squashName n
           in   Just $ SuperDef
                 { superDefModuleName   = C.ModuleName ["DDC"]
                        -- TODO: we don't have module names at use sites for the reify# prim,
                        -- for now all supers are tagged with the same dummy module name.
                 , superDefName         = txName
                 , superDefParams       = length csValue
                 , superDefBoxes        = length csBox
                 , superDefExp          = x }

        -- Some top level thing that doesn't look like a super.
        -- This is probably a CAF.
        slurpSuperDef _ _ = Nothing

        -- TODO: this is a copy-pasto from PrimCall. Put it somewhere shared.
        squashName (D.NameVar tx)   = tx
        squashName (D.NameExt n tx) = squashName n <> "$" <> tx
        squashName nSuper
         = error $ "ddc-core-discus.slurpSuperDefs: invalid super name " ++ show nSuper


-- | Inject the info table initialization function into the end of the module.
injectInfoInit
        :: C.ModuleName         -- ^ Name of the module we're making the table for.
        -> [C.ModuleName]       -- ^ Also call the init function for these other modules.
        -> [C.DataDef D.Name]   -- ^ Descriptions of data types defined locally in the module.
        -> [SuperDef a]         -- ^ Descriptions of supers defined locally in the module.
        -> C.Exp a D.Name       -- ^ Body expression of the module to inject into.
        -> C.Exp a D.Name

injectInfoInit modName mnsImport dataDefs superDefs xx
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
                $ makeInfoInitForSupers   a' superDefs
                $ D.xUnit a'


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
-- | Create a new frame to hold info about the given data type definitions,
--   and push it on the frame stack.
makeInfoInitForDataDefs
        :: a -> C.ModuleName -> [C.DataDef D.Name]
        -> C.Exp a D.Name -> C.Exp a D.Name

makeInfoInitForDataDefs a modName dataDefs xBase
 | null dataDefs = xBase
 | otherwise
 = foldr (C.XLet a) xBase
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
        flatName           = intercalate "." parts

        makeFrame ctors
         =  [ C.LLet (C.BAnon D.tAddr)
                $ D.xInfoFrameNew a (length ctors) ]

         ++ [ C.LLet (C.BNone (D.tWord 32))
                $  D.xInfoFrameAddData a
                        (C.XVar a (C.UIx 0))
                        iTag (length $ C.dataCtorFieldTypes ctor)
                        (C.XCon a (C.DaConPrim (D.NameLitTextLit (T.pack flatName))))
                        (C.XCon a (C.DaConPrim (textLitOfConName $ C.dataCtorName ctor)))
                | ctor  <- ctors
                | iTag  <- [0..]]

         ++ [ C.LLet (C.BNone D.tUnit)
                $ D.xInfoFramePush a (C.XVar a (C.UIx 0)) ]


---------------------------------------------------------------------------------------------------
-- | Create a new frame to hold info about the given supers,
--   and push it on the frame stack.
makeInfoInitForSupers
        :: a -> [SuperDef a]
        -> C.Exp a D.Name -> C.Exp a D.Name

makeInfoInitForSupers a superDefs xBase
 | null superDefs = xBase
 | otherwise
 = let
        lFrameNew
         = C.LLet (C.BAnon D.tAddr)
         $ D.xInfoFrameNew a (length superDefs)

        cEncode = C.Encode.Config
                { C.Encode.configTakeRef        = D.Encode.takeName
                , C.Encode.configTakeVarName    = D.Encode.takeVarName
                , C.Encode.configTakeConName    = D.Encode.takeConName }

        lsAddSuper
         = [ let C.ModuleName parts = superDefModuleName super
                 flatName = intercalate "." parts

                 (w0, w1, w2, w3)
                   = C.hashExpAsWord64s cEncode $ superDefExp super

             in  C.LLet (C.BNone (D.tWord 32))
                  $ D.xInfoFrameAddSuper a
                        (C.XVar a (C.UIx 0))
                        (superDefParams super) (superDefBoxes super)
                        (C.XCon a (C.DaConPrim (D.NameLitTextLit (T.pack flatName))))
                        (C.XCon a (C.DaConPrim (D.NameLitTextLit (superDefName super))))
                        w0 w1 w2 w3
           | super <- superDefs ]

        lFramePush
         = C.LLet (C.BNone D.tUnit)
         $ D.xInfoFramePush a (C.XVar a (C.UIx 0))

   in   foldr (C.XLet a) xBase
         $ lFrameNew : lsAddSuper ++ [lFramePush]


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
                { C.importValueSeaModuleName    = C.ModuleName ["DDC", "Internal", "Runtime"]
                , C.importValueSeaNameInternal  = nHookHandleTopLevel
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
                C.XLRec a bxs x2  -> C.XLRec a (concatMap downBind bxs) (downTop x2)
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
         , C.XAbs a p@(C.MTerm bParam) xBody <- x
         = [ ( C.BName n tMain
             , C.XAbs a p
                $ C.XBox a
                $ C.XLet a (C.LLet  (C.BNone D.tUnit)
                                  (C.xApps a
                                        (C.XVar a (C.UName (D.NameVar "_init$Main")))
                                        [C.RTerm $ D.xUnit a]))
                $ C.XRun a
                $ C.xApps a (C.xApps a (C.XVar a (C.UName nHookHandleTopLevel)) [C.RType tEff])
                          [C.RTerm (C.XApp a (C.XVar a (C.UName nMainUser)) (C.RTerm (D.xUnit a)))])

           , ( C.BName nMainUser tMain
             , C.XAbs a (C.MTerm bParam) xBody)
           ]

         | otherwise
         = error "ddc-core-discus.initializeModule: invalid type for main binding"

        downBind (b, x)
         = [(b, x)]

        nMainUser = D.NameVar "_main$start"


