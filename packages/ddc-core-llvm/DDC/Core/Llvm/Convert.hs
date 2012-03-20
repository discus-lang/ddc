
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Llvm.Type
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Module
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import DDC.Base.Pretty
import Control.Monad.State.Strict       (evalState)


-- Module -----------------------------------------------------------------------------------------
convertModule :: Module () Name -> LlvmModule
convertModule mm
        = evalState (llvmOfModuleM (defaultPlatform 4) mm) llvmStateInit


llvmOfModuleM 
        :: Platform
        -> Module () Name 
        -> LlvmM LlvmModule

llvmOfModuleM platform mm@(ModuleCore{})
 | [LRec bxs]   <- moduleLets mm   
 =      return  $ LlvmModule 
                { modComments   = []
                , modAliases    = []
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = map (uncurry (llvmFunctionOfSuper platform)) bxs }

 | otherwise    = die "invalid module"


-- Super ------------------------------------------------------------------------------------------
-- | Convert a top-level supercombinator to LLVM.
llvmFunctionOfSuper :: Platform -> Bind Name -> Exp () Name -> LlvmFunction
llvmFunctionOfSuper platform (BName n tSuper) x
 | Just (bsParam, _xBody)     <- takeXLams x
 = let  
        -- Split off the argument and result types.
        (tsArgs, tResult)       
                = takeTFunArgResult tSuper

        -- Declaration of the super.
        decl    = LlvmFunctionDecl 
                { decName               = renderPlain $ ppr n
                , decLinkage            = External
                , decCallConv           = CC_Ccc
                , decReturnType         = toLlvmType platform tResult
                , decParamListType      = FixedArgs
                , decParams             = map (llvmParameterOfType platform) tsArgs
                , decAlign              = LlvmAlignBytes (platformAlignFunctions platform) }

   in   LlvmFunction
                { funcDecl              = decl
                , funcParams            = map llvmNameOfParam bsParam
                , funcAttrs             = [] 
                , funcSect              = LlvmSectionAuto
                , funcBody              = [] }

llvmFunctionOfSuper _ _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
llvmNameOfParam :: Bind Name -> String
llvmNameOfParam bb
 = case bb of
        BName n _       -> renderPlain $ ppr n
        _               -> die "invalid parameter name"


