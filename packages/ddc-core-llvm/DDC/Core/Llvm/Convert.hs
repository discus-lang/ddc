
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Core.Llvm.LlvmM
import DDC.Core.Module
import DDC.Core.Sea.Output.Name
import DDC.Llvm.Module
-- import DDC.Base.Pretty
import Control.Monad.State.Strict       (evalState)

-- | Called when we find a thing that cannot be converted to C.
-- die :: String -> a
-- die msg = error $ "DDC.Core.Llvm.Convert " ++ msg


-- Module ---------------------------------------------------------------------
convertModule :: Module () Name -> LlvmModule
convertModule mm
        = evalState (convertModuleM mm) llvmStateInit


convertModuleM 
        :: Module () Name 
        -> LlvmM LlvmModule

convertModuleM _mm@(ModuleCore{})
 = do
        return  $ LlvmModule 
                { modComments   = []
                , modAliases    = []
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = [] }


