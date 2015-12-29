
module DDC.Core.Llvm.Convert.Exp.PrimCall
        (convPrimCall)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import Data.Sequence            (Seq)
import qualified DDC.Core.Exp           as C
import qualified DDC.Core.Salt          as A
import qualified DDC.Core.Salt.Exp      as A
import qualified Data.Sequence          as Seq


-- | Convert a primitive store operation to LLVM.
convPrimCall
        :: Show a
        => Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Prim to call.
        -> C.Type A.Name        -- ^ Type of prim.
        -> [C.Exp a A.Name]     -- ^ Arguments to prim.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimCall ctx mDst p _tPrim xs0
 = let  pp              = contextPlatform ctx
        Right xs        = sequence $ fmap A.fromAnnot xs0
   in case p of
        A.PrimCall (A.PrimCallStd arity)
         | Just (mFun : msArgs) <- sequence $ map (mconvAtom ctx) xs
         -> Just $ do
                xFun'   <- mFun
                xsArgs' <- sequence msArgs

                vFun@(Var nFun _) 
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

