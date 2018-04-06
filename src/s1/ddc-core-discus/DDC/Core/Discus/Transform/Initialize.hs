{-# LANGUAGE OverloadedStrings #-}

-- | Add code to initialize the module.
module DDC.Core.Discus.Transform.Initialize
        (initializeModule)
where
import qualified DDC.Type.Exp.Simple            as C
import qualified DDC.Core.Exp.Annot             as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Salt.Runtime          as A
import qualified DDC.Core.Discus                as D
import qualified DDC.Core.Discus.Compounds      as D


---------------------------------------------------------------------------------------------------
initializeModule
        :: A.Config
        -> C.Module a D.Name -> C.Module a D.Name

initializeModule config mm
 | C.isMainModuleName $ C.moduleName mm
 = initializeMain config mm

 | otherwise
 = mm


---------------------------------------------------------------------------------------------------
-- | Insert initialization code into the main module.
--
--   We insert the call to the top level exception handler only when

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
        downBind (C.BName n@(D.NameVar "main") tMain, x)
         | Just (_tParam, tSusp)              <- C.takeTFun tMain
         , Just (tEff,   _tResult)            <- C.takeTSusp tSusp
         , C.XAbs a p@(C.MTerm _bParam) xBody <- x
         = ( C.BName n tMain
           , C.XAbs a p
              $ C.XBox a $ C.XRun a
              $ C.xApps a (C.xApps a (C.XVar a (C.UName nHookHandleTopLevel)) [C.RType tEff])
                          [C.RTerm xBody])

         | otherwise
         = error "ddc-core-discus.initializeModule: invalid type for main binding"

        downBind (b, x)
         = (b, x)

