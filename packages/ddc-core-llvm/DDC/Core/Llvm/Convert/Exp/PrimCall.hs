
module DDC.Core.Llvm.Convert.Exp.PrimCall
        (convPrimCall)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.LlvmM
import Data.Sequence            (Seq)
import qualified DDC.Core.Exp   as C
import qualified DDC.Core.Salt  as A
import qualified Data.Sequence  as Seq


-- | Convert a primitive store operation to LLVM.
convPrimCall
        :: Show a
        => Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> Maybe (LlvmM (Seq AnnotInstr))

convPrimCall ctx mDst p _tPrim xs
 = let  pp      = contextPlatform ctx
   in case p of
        A.PrimCall (A.PrimCallStd arity)
         | Just (xFun' : xsArgs')        <- mconvAtoms ctx xs
         -> Just 
          $ do  vFun@(Var nFun _) 
                        <- newUniqueNamedVar "fun" 
                        $  TPointer $ tFunction (replicate arity (tAddr pp)) (tAddr pp)

                return  $ Seq.fromList $ map annotNil
                        [ IConv vFun (ConvInttoptr) xFun'
                        , ICall mDst CallTypeStd Nothing
                                      (tAddr pp) nFun xsArgs' []]

        _ -> Nothing


tFunction :: [Type] -> Type -> Type
tFunction tsArgs tResult
        = TFunction
        $ FunctionDecl
        { declName              = "anon"
        , declLinkage           = External
        , declCallConv          = CC_Ccc
        , declReturnType        = tResult
        , declParamListType     = FixedArgs
        , declParams            = [ Param t [] | t <- tsArgs ]
        , declAlign             = AlignNone }

