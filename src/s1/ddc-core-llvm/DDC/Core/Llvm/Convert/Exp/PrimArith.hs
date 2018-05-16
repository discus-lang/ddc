
module DDC.Core.Llvm.Convert.Exp.PrimArith
        (convPrimArith)
where
import DDC.Llvm.Syntax
import DDC.Core.Llvm.Convert.Exp.Atom
import DDC.Core.Llvm.Convert.Context
import DDC.Core.Llvm.Convert.Type
import DDC.Core.Llvm.Convert.Base
import Data.Sequence                    (Seq)
import qualified DDC.Core.Exp           as C
import qualified DDC.Core.Salt          as A
import qualified Data.Sequence          as Seq


-- | Convert a primitive call to LLVM,
--   or Nothing if this doesn't look like such an operation.

-- TODO: refactor this mess, and eliminate the hard call to error.
convPrimArith
        :: Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Primitive to call.
        -> [A.Arg]              -- ^ Arguments to primitive.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimArith ctx mdst p xs
 = let  pp      = contextPlatform ctx
        kenv    = contextKindEnv  ctx
   in case p of
        -- Unary operators ------------
        A.PrimArith op
         | A.RType t : args     <- xs
         , Just dst             <- mdst
         , Just [mx1]           <- sequence $ map (mconvArg ctx) args
         -> Just $ do
                x1'     <- mx1
                t'      <- convertType pp kenv t
                let result
                     | A.PrimArithNeg <- op
                     , isIntegralT t
                     = return $ IOp dst OpSub (XLit $ LitInt t' 0) x1'

                     | A.PrimArithNeg <- op
                     , isFloatingT t
                     = return $ IOp dst OpSub (XLit $ LitFloat t' 0) x1'

                     -- Cannot use primop at this type.
                     | otherwise
                     = throw  $ ErrorInvalidArith op t

                instr  <- result
                return $ Seq.singleton (annotNil instr)

        -- Binary operators -----------
        A.PrimArith op
         | A.RType t : args   <- xs
         , Just dst             <- mdst
         , Just [mx1, mx2]      <- sequence $ map (mconvArg ctx) args
         -> Just $ do
                x1'     <- mx1
                x2'     <- mx2
                let result
                     | Just op'     <- convPrimArith2 op t
                     = return $ IOp dst op' x1' x2'

                     | Just icond'  <- convPrimICond op t
                     = return $ ICmp dst (ICond icond') x1' x2'

                     | Just fcond'  <- convPrimFCond op t
                     = return $ ICmp dst (FCond fcond') x1' x2'

                     -- Cannot use primop at this type.
                     | otherwise
                     = throw  $ ErrorInvalidArith op t

                instr  <- result
                return $ Seq.singleton (annotNil instr)

        A.PrimArith op
         | Just dst             <- mdst
         , Just [mx1, mx2]      <- sequence $ map (mconvArg ctx) xs
         -> Just $ do
                x1'     <- mx1
                x2'     <- mx2
                let result
                     | A.PrimArithOr    <- op
                     = return $ IOp dst OpOr x1' x2'

                     | otherwise
                     = error "convPrimArith: failed"

                instr <- result
                return $ Seq.singleton (annotNil instr)


        -- This doesn't look like an arithmetic primop.
        _ -> Nothing


-- | Convert a binary primop from Core Sea to LLVM form.
convPrimArith2 :: A.PrimArith -> C.Type A.Name -> Maybe Op
convPrimArith2 op t
 = case op of
        A.PrimArithNeg                  -> Nothing

        A.PrimArithAdd
         | isIntegralT t                -> Just OpAdd
         | isFloatingT t                -> Just OpFAdd
         | otherwise                    -> Nothing

        A.PrimArithSub
         | isIntegralT t                -> Just OpSub
         | isFloatingT t                -> Just OpFSub
         | otherwise                    -> Nothing

        A.PrimArithMul
         | isIntegralT t                -> Just OpMul
         | isFloatingT t                -> Just OpFMul
         | otherwise                    -> Nothing

        -- ISSUE #450 Division by Zero in LLVM generated code has undefined behaviour.
        A.PrimArithDiv
         | isIntegralT t, isUnsignedT t -> Just OpUDiv
         | isIntegralT t, isSignedT t   -> Just OpSDiv
         | isFloatingT t                -> Just OpFDiv
         | otherwise                    -> Nothing

        -- ISSUE #450 Division by Zero in LLVM generated code has undefined behaviour.
        A.PrimArithRem
         | isIntegralT t, isUnsignedT t -> Just OpURem
         | isIntegralT t, isSignedT t   -> Just OpSRem
         | isFloatingT t                -> Just OpFRem
         | otherwise                    -> Nothing

        -- ISSUE #451 The 'mod' function in LLVM generated code is not implemented.
        A.PrimArithMod                  -> Nothing

        -- comparison -------------------------------------
        -- These are handled via the ICmp instruction rather that IOp.
        A.PrimArithEq                   -> Nothing
        A.PrimArithNeq                  -> Nothing
        A.PrimArithGt                   -> Nothing
        A.PrimArithGe                   -> Nothing
        A.PrimArithLt                   -> Nothing
        A.PrimArithLe                   -> Nothing

        -- boolean ----------------------------------------
        A.PrimArithAnd
         | isBoolT t                    -> Just OpAnd
         | otherwise                    -> Nothing

        A.PrimArithOr
         | isBoolT t                    -> Just OpOr
         | otherwise                    -> Nothing

        -- bitwise ----------------------------------------
        A.PrimArithShl
         | isIntegralT t                -> Just OpShl
         | otherwise                    -> Nothing

        A.PrimArithShr
         | isIntegralT t, isUnsignedT t -> Just OpLShr
         | isIntegralT t, isSignedT t   -> Just OpAShr
         | otherwise                    -> Nothing

        A.PrimArithBAnd
         | isIntegralT t                -> Just OpAnd
         | otherwise                    -> Nothing

        A.PrimArithBOr
         | isIntegralT t                -> Just OpOr
         | otherwise                    -> Nothing

        A.PrimArithBXOr
         | isIntegralT t                -> Just OpXor
         | otherwise                    -> Nothing


-- | Convert an integer comparison from Core Sea to LLVM form.
convPrimICond :: A.PrimArith -> C.Type A.Name -> Maybe ICond
convPrimICond op t
 | isIntegralT t
 = case op of
        A.PrimArithEq                   -> Just ICondEq
        A.PrimArithNeq                  -> Just ICondNe
        A.PrimArithGt                   -> Just ICondUgt
        A.PrimArithGe                   -> Just ICondUge
        A.PrimArithLt                   -> Just ICondUlt
        A.PrimArithLe                   -> Just ICondUle
        _                               -> Nothing

 | otherwise = Nothing


-- | Convert a floating point comparison from Core Sea to LLVM form.
convPrimFCond :: A.PrimArith -> C.Type A.Name -> Maybe FCond
convPrimFCond op t
 | isFloatingT t
 = case op of
        A.PrimArithEq                   -> Just FCondOeq
        A.PrimArithNeq                  -> Just FCondOne
        A.PrimArithGt                   -> Just FCondOgt
        A.PrimArithGe                   -> Just FCondOge
        A.PrimArithLt                   -> Just FCondOlt
        A.PrimArithLe                   -> Just FCondOle
        _                               -> Nothing

 | otherwise = Nothing

