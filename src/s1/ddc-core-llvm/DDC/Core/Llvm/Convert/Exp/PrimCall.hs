
module DDC.Core.Llvm.Convert.Exp.PrimCall
        (convPrimCall)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Base
import Data.Sequence            (Seq)
import qualified DDC.Core.Salt          as A
import qualified Data.Sequence          as Seq


-- | Convert a primitive store operation to LLVM.
convPrimCall
        :: Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Prim to call.
        -> [A.Arg]              -- ^ Arguments to prim.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimCall ctx mDst p xs
 = let  pp              = contextPlatform ctx
   in case p of
        A.PrimControl (A.PrimControlCall arity)
         | xFun : xsArgs <- drop (arity + 1) xs
         , Just mFun     <- mconvArg ctx xFun
         , Just msArgs   <- sequence $ map (mconvArg ctx) xsArgs
         -> Just $ do
                xFun'   <- mFun
                xsArgs' <- sequence msArgs

                vFun@(Var nFun _)
                        <- newUniqueNamedVar "fun"
                        $  TPointer $ tFunction (replicate arity (tPtr (tObj pp))) (tPtr (tObj pp))

                return  $ Seq.fromList $ map annotNil
                        [ IConv vFun (ConvInttoptr) xFun'
                        , ICall mDst CallTypeStd Nothing
                                      (tPtr (tObj pp)) nFun xsArgs' []]

        _ -> Nothing


-- Build the type of a function with the given arguments and result type.
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
        , declAlign             = AlignNone
        , declGarbageCollector  = Just "shadow-stack" }

