
module DDC.Core.Llvm.Convert
        (convertModule)
where
import DDC.Llvm.Attr
import DDC.Llvm.Stmt
import DDC.Llvm.Function
import DDC.Llvm.Module
import DDC.Llvm.Type
import DDC.Llvm.Var
import DDC.Core.Llvm.Runtime.Object
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Platform
import DDC.Core.Llvm.LlvmM
import DDC.Core.Compounds
import DDC.Core.Sea.Output.Name
import DDC.Type.Compounds
import DDC.Base.Pretty
import Control.Monad.State.Strict       (evalState)
import qualified DDC.Core.Module        as C
import qualified DDC.Core.Exp           as C
import Control.Monad.State.Strict       (gets)


-- Module -----------------------------------------------------------------------------------------
convertModule :: C.Module () Name -> Module
convertModule mm
        = evalState (llvmOfModuleM mm) 
        $ llvmStateInit (defaultPlatform 4)


llvmOfModuleM 
        :: C.Module () Name 
        -> LlvmM Module

llvmOfModuleM mm@(C.ModuleCore{})
 | [C.LRec bxs]         <- C.moduleLets mm   
 = do   platform        <- gets llvmStatePlatform
        functions       <- mapM (uncurry (llvmFunctionOfSuper)) bxs
        return  $ Module 
                { modComments   = []
                , modAliases    = [aObj platform]
                , modGlobals    = []
                , modFwdDecls   = []
                , modFuncs      = functions }

 | otherwise    = die "invalid module"


-- Super ------------------------------------------------------------------------------------------
-- | Convert a top-level supercombinator to LLVM.
llvmFunctionOfSuper 
        :: C.Bind Name 
        -> C.Exp () Name 
        -> LlvmM Function

llvmFunctionOfSuper (C.BName n tSuper) x
 | Just (bsParam, xBody)     <- takeXLams x
 = do   platform        <- gets llvmStatePlatform

        -- Split off the argument and result types.
        let (tsArgs, tResult)       
                = takeTFunArgResult tSuper

        -- Declaration of the super.
        let decl 
                = FunctionDecl 
                { declName               = renderPlain $ ppr n
                , declLinkage            = External
                , declCallConv           = CC_Ccc
                , declReturnType         = toLlvmType platform tResult
                , declParamListType      = FixedArgs
                , declParams             = map (llvmParameterOfType platform) tsArgs
                , declAlign              = AlignBytes (platformAlignFunctions platform) }

        blockId <- newUniqueBlockId
        blocks  <- llvmBlocksOfBody [] blockId [] xBody

        return  $ Function
                { functionDecl           = decl
                , functionParams         = map llvmNameOfParam bsParam
                , functionAttrs          = [] 
                , functionSection        = SectionAuto
                , functionBlocks         = blocks }

llvmFunctionOfSuper _ _
        = die "invalid super"


-- | Take the string name to use for a function parameter.
llvmNameOfParam :: C.Bind Name -> String
llvmNameOfParam bb
 = case bb of
        C.BName n _     -> renderPlain $ ppr n
        _               -> die "invalid parameter name"


-- Body --------------------------------------------------------------------------------------------
llvmBlocksOfBody 
        :: [Block]              -- ^ Previous blocks.
        -> BlockId              -- ^ Id of current block.
        -> [Stmt]               -- ^ Stmts in current block.
        -> C.Exp () Name        -- ^ Expression being converted.
        -> LlvmM [Block]        -- ^ Final blocks of function body.

llvmBlocksOfBody blocks blockId stmts xx
 = do   platform        <- gets llvmStatePlatform
        case xx of

         -- Final return statement.
         C.XApp{}
          |  Just (NamePrim p, xs)              <- takeXPrimApps xx
          ,  PrimControl PrimControlReturn      <- p
          ,  [C.XType t, x]                     <- xs
          ,  Just v                             <- takeVarOfTypedExp platform t x
          -> return  $  blocks 
                     ++ [Block blockId (reverse stmts ++ [SReturn (Just v)])]

         C.XLet _ (C.LLet C.LetStrict (C.BName (NameVar _s) _t) _x1) x2
          ->  llvmBlocksOfBody blocks blockId stmts x2


         -- TODO: Debugging only
         _ -> return $  blocks
                     ++ [Block blockId (reverse stmts ++ [SUnreachable])]

         -- die "invalid body statement"

takeVarOfTypedExp :: Platform -> C.Type Name -> C.Exp () Name -> Maybe Var
takeVarOfTypedExp platform t xx
 = case xx of
        C.XVar _ (C.UName (NameVar s) _)    
          -> Just $ VarLocal s (toLlvmType platform t)

        _ -> Nothing

