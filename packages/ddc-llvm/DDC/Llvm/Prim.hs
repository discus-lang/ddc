
module DDC.Llvm.Prim
        ( Op    (..)
        , ICond (..)
        , FCond (..)
        , Conv  (..))
where
import DDC.Base.Pretty


-- BinOp -----------------------------------------------------------------------------------------
-- | Binary arithmetic operators.
data Op
        = OpAdd     -- ^ add two integers, floating point or vector values.
        | OpSub     -- ^ subtract two ...
        | OpMul     -- ^ multiply ..
        | OpUDiv    -- ^ unsigned integer or vector division.
        | OpSDiv    -- ^ signed integer ..
        | OpURem    -- ^ unsigned integer or vector remainder (mod)
        | OpSRem    -- ^ signed ...

        | OpFAdd    -- ^ add two floating point or vector values.
        | OpFSub    -- ^ subtract two ...
        | OpFMul    -- ^ multiply ...
        | OpFDiv    -- ^ divide ...
        | OpFRem    -- ^ remainder ...

        | OpShl     -- ^ Left shift.
        | OpLShr    -- ^ Logical shift right
        | OpAShr    -- ^ Arithmetic shift right.
                        --   The most significant bits of the result will be equal to the
                        --   sign bit of the left operand.

        | OpAnd     -- ^ AND bitwise logical operation.
        | OpOr      -- ^ OR bitwise logical operation.
        | OpXor     -- ^ XOR bitwise logical operation.
        deriving (Eq, Show)


instance Pretty Op where
 ppr op
  = case op of
        OpAdd       -> text "add"
        OpSub       -> text "sub"
        OpMul       -> text "mul"
        OpUDiv      -> text "udiv"
        OpSDiv      -> text "sdiv"
        OpURem      -> text "urem"
        OpSRem      -> text "srem"
        OpFAdd      -> text "fadd"
        OpFSub      -> text "fsub"
        OpFMul      -> text "fmul"
        OpFDiv      -> text "fdiv"
        OpFRem      -> text "frem"
        OpShl       -> text "shl"
        OpLShr      -> text "lshr"
        OpAShr      -> text "ashr"
        OpAnd       -> text "and"
        OpOr        -> text "or"
        OpXor       -> text "xor"


-- PrimICond --------------------------------------------------------------------------------------
-- | Integer comparison.
data ICond
        = ICondEq       -- ^ Equal (Signed and Unsigned)
        | ICondNe       -- ^ Not equal (Signed and Unsigned)
        | ICondUgt      -- ^ Unsigned greater than
        | ICondUge      -- ^ Unsigned greater than or equal
        | ICondUlt      -- ^ Unsigned less than
        | ICondUle      -- ^ Unsigned less than or equal
        | ICondSgt      -- ^ Signed greater than
        | ICondSge      -- ^ Signed greater than or equal
        | ICondSlt      -- ^ Signed less than
        | ICondSle      -- ^ Signed less than or equal
        deriving (Eq, Show)

instance Pretty ICond where
 ppr pp
  = case pp of
        ICondEq         -> text "eq"
        ICondNe         -> text "ne"
        ICondUgt        -> text "ugt"
        ICondUge        -> text "uge"
        ICondUlt        -> text "ult"
        ICondUle        -> text "ule"
        ICondSgt        -> text "sgt"
        ICondSge        -> text "sge"
        ICondSlt        -> text "slt"
        ICondSle        -> text "sle"


-- PrimFCond --------------------------------------------------------------------------------------
data FCond
        = FCondFalse    -- ^ Always yields false, regardless of operands.
        | FCondOeq      -- ^ Both operands are not a QNAN and op1 is equal to op2.
        | FCondOgt      -- ^ Both operands are not a QNAN and op1 is greater than op2.
        | FCondOge      -- ^ Both operands are not a QNAN and op1 is greater than or equal to op2.
        | FCondOlt      -- ^ Both operands are not a QNAN and op1 is less than op2.
        | FCondOle      -- ^ Both operands are not a QNAN and op1 is less than or equal to op2.
        | FCondOne      -- ^ Both operands are not a QNAN and op1 is not equal to op2.
        | FCondOrd      -- ^ Both operands are not a QNAN.
        | FCondUeq      -- ^ Either operand is a QNAN or op1 is equal to op2.
        | FCondUgt      -- ^ Either operand is a QNAN or op1 is greater than op2.
        | FCondUge      -- ^ Either operand is a QNAN or op1 is greater than or equal to op2.
        | FCondUlt      -- ^ Either operand is a QNAN or op1 is less than op2.
        | FCondUle      -- ^ Either operand is a QNAN or op1 is less than or equal to op2.
        | FCondUne      -- ^ Either operand is a QNAN or op1 is not equal to op2.
        | FCondUno      -- ^ Either operand is a QNAN.
        | FCondTrue     -- ^ Always yields true, regardless of operands.
        deriving (Eq, Show)

instance Pretty FCond where
 ppr pp
  = case pp of
        FCondFalse      -> text "false"
        FCondOeq        -> text "oeq"
        FCondOgt        -> text "ogt"
        FCondOge        -> text "oge"
        FCondOlt        -> text "olt"
        FCondOle        -> text "ole"
        FCondOne        -> text "one"
        FCondOrd        -> text "ord"
        FCondUeq        -> text "ueq"
        FCondUgt        -> text "ugt"
        FCondUge        -> text "uge"
        FCondUlt        -> text "ult"
        FCondUle        -> text "ule"
        FCondUne        -> text "une"
        FCondUno        -> text "uno"
        FCondTrue       -> text "true"


-- PrimConv ---------------------------------------------------------------------------------------
-- | Conversion Operations
data Conv
        = ConvTrunc     -- ^ Integer truncate
        | ConvZext      -- ^ Integer extend (zero fill)
        | ConvSext      -- ^ Integer extend (sign fill)
        | ConvFptrunc   -- ^ Float truncate
        | ConvFpext     -- ^ Float extend
        | ConvFptoui    -- ^ Float to unsigned Integer
        | ConvFptosi    -- ^ Float to signed Integer
        | ConvUintofp   -- ^ Unsigned Integer to Float
        | ConvSintofp   -- ^ Signed Int to Float
        | ConvPtrtoint  -- ^ Pointer to Integer
        | ConvInttoptr  -- ^ Integer to Pointer
        | ConvBitcast   -- ^ Cast between types where no bit manipulation is needed
        deriving (Eq, Show)


instance Pretty Conv where
 ppr pp
  = case pp of
        ConvTrunc       -> text "trunc"
        ConvZext        -> text "zext"
        ConvSext        -> text "sext"
        ConvFptrunc     -> text "fptrunc"
        ConvFpext       -> text "fpext"
        ConvFptoui      -> text "fptoui"
        ConvFptosi      -> text "fptosi"
        ConvUintofp     -> text "uintofp"
        ConvSintofp     -> text "sintofp"
        ConvPtrtoint    -> text "ptrtoint"
        ConvInttoptr    -> text "inttoptr"
        ConvBitcast     -> text "bitcast"

