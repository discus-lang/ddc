
-- | Add code to initialize the module.
module DDC.Core.Discus.Transform.Initialize
        (initializeModule)
where
import qualified DDC.Type.Exp.Simple            as C
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
initializeMain
        :: A.Config
        -> C.Module a D.Name -> C.Module a D.Name

initializeMain (A.Config { A.configHookHandleTopLevel = Just txHookHandleTopLevel}) mm
 = mm   { C.moduleImportValues  = C.moduleImportValues mm ++ [ivHookHandleTopLevel] }
 where
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