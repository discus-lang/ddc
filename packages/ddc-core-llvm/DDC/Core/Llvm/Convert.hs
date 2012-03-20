
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Llvm.Type
import DDC.Llvm.Var
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import DDC.Base.Pretty
import Control.Monad.State.Strict       (evalState)
import qualified DDC.Core.Module        as C


-- Module -----------------------------------------------------------------------------------------
convertModule :: C.Module () Name -> Module
convertModule mm
        = evalState (llvmOfModuleM (defaultPlatform 4) mm) llvmStateInit


llvmOfModuleM 
        :: Platform
        -> C.Module () Name 
        -> LlvmM Module

llvmOfModuleM platform mm@(C.ModuleCore{})
 | [LRec bxs]   <- C.moduleLets mm   
 =      return  $ Module 
                { modComments   = []
                , modAliases    = []
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = map (uncurry (llvmFunctionOfSuper platform)) bxs }

 | otherwise    = die "invalid module"


-- Super ------------------------------------------------------------------------------------------
-- | Convert a top-level supercombinator to LLVM.
llvmFunctionOfSuper :: Platform -> Bind Name -> Exp () Name -> Function
llvmFunctionOfSuper platform (BName n tSuper) x
 | Just (bsParam, _xBody)     <- takeXLams x
 = let  
        -- Split off the argument and result types.
        (tsArgs, tResult)       
                = takeTFunArgResult tSuper

        -- Declaration of the super.
        decl    = FunctionDecl 
                { declName               = renderPlain $ ppr n
                , declLinkage            = External
                , declCallConv           = CC_Ccc
                , declReturnType         = toLlvmType platform tResult
                , declParamListType      = FixedArgs
                , declParams             = map (llvmParameterOfType platform) tsArgs
                , declAlign              = AlignBytes (platformAlignFunctions platform) }

   in   Function
                { functionDecl           = decl
                , functionParams         = map llvmNameOfParam bsParam
                , functionAttrs          = [] 
                , functionSection        = SectionAuto
                , functionBlocks         = [] }

llvmFunctionOfSuper _ _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
llvmNameOfParam :: Bind Name -> String
llvmNameOfParam bb
 = case bb of
        BName n _       -> renderPlain $ ppr n
        _               -> die "invalid parameter name"


