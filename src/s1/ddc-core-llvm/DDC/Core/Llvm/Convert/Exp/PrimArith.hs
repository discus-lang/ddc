
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
convPrimArith
        :: Context              -- ^ Context of the conversion.
        -> Maybe Var            -- ^ Assign result to this var.
        -> A.PrimOp             -- ^ Primitive to call.
        -> [A.Arg]              -- ^ Arguments to primitive.
        -> Maybe (ConvertM (Seq AnnotInstr))

convPrimArith ctx mdst p xs
 | pp           <- contextPlatform ctx
 , kenv         <- contextKindEnv  ctx
 , Just dst     <- mdst
 , ts           <- [t | A.RType t <- xs]
 , xsArg        <- drop (length ts) xs
 , Just mxs     <- sequence $ map (mconvArg ctx) xsArg
 , A.PrimArith op <- p
 = Just $ do
        ts'     <- mapM (convertType pp kenv )ts
        xsArg'  <- sequence mxs

        case convPrimArithArgs dst op ts ts' xsArg' of
         Nothing    -> throw  $ ErrorInvalidArith op ts
         Just instr -> return $ Seq.singleton (annotNil instr)

 | otherwise    = Nothing


-- | Produce an LLVM instruction to implement the given operator.
--   We dispatch to functions to handle each call pattern.
convPrimArithArgs
        :: Var                  -- ^ Assign result to this variable.
        -> A.PrimArith          -- ^ Arithmetic operator being applied.
        -> [C.Type A.Name]      -- ^ Core version of type arguments.
        -> [Type]               -- ^ LLVM version of type arguments.
        -> [Exp]                -- ^ LLVM version of term arguments.
        -> Maybe Instr          -- ^ Result instruction, if the conversion succeeded.

convPrimArithArgs dst op ts ts' xs'
 = case (ts, ts', xs') of
        ([t1], [t1'],  [x1'])      -> convTX  t1 t1' x1'
        ([],   [],     [x1', x2']) -> convXX  x1' x2'
        ([t1], [_t1'], [x1', x2']) -> convTXX t1  x1' x2'
        _                          -> Nothing

 where
  isIntT   t = isIntegralT t
  isFloatT t = isFloatingT t

  isSIntT  t = isIntegralT t && isSignedT t
  isUIntT  t = isIntegralT t && isUnsignedT t

  convTX t1 t1' x1'
   = case op of
        A.PrimArithNeg
         |  isIntT   t1 -> Just $ IOp dst OpSub (XLit $ LitInt t1' 0) x1'
         |  isFloatT t1 -> Just $ IOp dst OpSub (XLit $ LitFloat t1' 0) x1'
        _               -> Nothing

  convXX x1' x2'
   = case op of
        A.PrimArithOr   -> Just $ IOp dst OpOr   x1' x2'
        A.PrimArithAnd  -> Just $ IOp dst OpAnd  x1' x2'
        _               -> Nothing

  convTXX t1 x1' x2'
   = case op of
        A.PrimArithAdd
         | isIntT   t1  -> Just $ IOp dst OpAdd  x1' x2'
         | isFloatT t1  -> Just $ IOp dst OpFAdd x1' x2'

        A.PrimArithSub
         | isIntT   t1  -> Just $ IOp dst OpSub  x1' x2'
         | isFloatT t1  -> Just $ IOp dst OpFSub x1' x2'

        A.PrimArithMul
         | isIntT   t1  -> Just $ IOp dst OpMul  x1' x2'
         | isFloatT t1  -> Just $ IOp dst OpFMul x1' x2'

        -- ISSUE #450: Division by Zero in LLVM generated code has undefined behaviour.
        A.PrimArithDiv
         | isUIntT  t1  -> Just $ IOp dst OpUDiv x1' x2'
         | isSIntT  t1  -> Just $ IOp dst OpSDiv x1' x2'
         | isFloatT t1  -> Just $ IOp dst OpFDiv x1' x2'

        -- ISSUE #450: Division by Zero in LLVM generated code has undefined behaviour.
        A.PrimArithRem
         | isUIntT  t1  -> Just $ IOp dst OpURem x1' x2'
         | isSIntT  t1  -> Just $ IOp dst OpSRem x1' x2'
         | isFloatT t1  -> Just $ IOp dst OpFRem x1' x2'

        -- ISSUE #451: The 'mod' function in LLVM gnerated code is not implemented.
        A.PrimArithMod  -> Nothing

        A.PrimArithEq
         | isIntT   t1  -> Just $ ICmp dst (ICond ICondEq)  x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOeq) x1' x2'

        A.PrimArithNeq
         | isIntT   t1  -> Just $ ICmp dst (ICond ICondNe)  x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOne) x1' x2'

        A.PrimArithGt
         | isUIntT  t1  -> Just $ ICmp dst (ICond ICondUgt) x1' x2'
         | isSIntT  t1  -> Just $ ICmp dst (ICond ICondSgt) x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOgt) x1' x2'

        A.PrimArithGe
         | isUIntT  t1  -> Just $ ICmp dst (ICond ICondUge) x1' x2'
         | isSIntT  t1  -> Just $ ICmp dst (ICond ICondSge) x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOge) x1' x2'

        A.PrimArithLt
         | isUIntT  t1  -> Just $ ICmp dst (ICond ICondUlt) x1' x2'
         | isSIntT  t1  -> Just $ ICmp dst (ICond ICondSlt) x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOlt) x1' x2'

        A.PrimArithLe
         | isUIntT  t1  -> Just $ ICmp dst (ICond ICondUle) x1' x2'
         | isSIntT  t1  -> Just $ ICmp dst (ICond ICondSle) x1' x2'
         | isFloatT t1  -> Just $ ICmp dst (FCond FCondOle) x1' x2'

        A.PrimArithShl
         | isIntT   t1  -> Just $ IOp  dst OpShl  x1' x2'

        A.PrimArithShr
         | isUIntT  t1  -> Just $ IOp  dst OpLShr x1' x2'
         | isSIntT  t1  -> Just $ IOp  dst OpAShr x1' x2'

        A.PrimArithBAnd
         | isIntT   t1  -> Just $ IOp  dst OpAnd  x1' x2'

        A.PrimArithBOr
         | isIntT   t1  -> Just $ IOp  dst OpOr   x1' x2'

        A.PrimArithBXOr
         | isIntT   t1  -> Just $ IOp  dst OpXor  x1' x2'

        _               -> Nothing

