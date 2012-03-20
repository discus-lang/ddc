
module DDC.Llvm.Prim
        ( MachOp    (..)
        , CmpOp     (..)
        , CastOp    (..))
where
import DDC.Base.Pretty


-- MachOp -----------------------------------------------------------------------------------------
-- | Llvm binary operators machine operations.
data MachOp
        = MachOpAdd     -- ^ add two integers, floating point or vector values.
        | MachOpSub     -- ^ subtract two ...
        | MachOpMul     -- ^ multiply ..
        | MachOpUDiv    -- ^ unsigned integer or vector division.
        | MachOpSDiv    -- ^ signed integer ..
        | MachOpURem    -- ^ unsigned integer or vector remainder (mod)
        | MachOpSRem    -- ^ signed ...

        | MachOpFAdd    -- ^ add two floating point or vector values.
        | MachOpFSub    -- ^ subtract two ...
        | MachOpFMul    -- ^ multiply ...
        | MachOpFDiv    -- ^ divide ...
        | MachOpFRem    -- ^ remainder ...

        | MachOpShl     -- ^ Left shift.
        | MachOpLShr    -- ^ Logical shift right
        | MachOpAShr    -- ^ Arithmetic shift right.
                        -- The most significant bits of the result will be equal to the sign bit of
                        -- the left operand.


        | MachOpAnd     -- ^ AND bitwise logical operation.
        | MachOpOr      -- ^ OR bitwise logical operation.
        | MachOpXor     -- ^ XOR bitwise logical operation.
        deriving (Eq, Show)


instance Pretty MachOp where
 ppr op
  = case op of
        MachOpAdd       -> text "add"
        MachOpSub       -> text "sub"
        MachOpMul       -> text "mul"
        MachOpUDiv      -> text "udiv"
        MachOpSDiv      -> text "sdiv"
        MachOpURem      -> text "urem"
        MachOpSRem      -> text "srem"
        MachOpFAdd      -> text "fadd"
        MachOpFSub      -> text "fsub"
        MachOpFMul      -> text "fmul"
        MachOpFDiv      -> text "fdiv"
        MachOpFRem      -> text "frem"
        MachOpShl       -> text "shl"
        MachOpLShr      -> text "lshr"
        MachOpAShr      -> text "ashr"
        MachOpAnd       -> text "and"
        MachOpOr        -> text "or"
        MachOpXor       -> text "xor"


-- CmpOp ------------------------------------------------------------------------------------------
-- | Llvm compare operations.
data CmpOp
        = CmpOpEq     -- ^ Equal (Signed and Unsigned)
        | CmpOpNe     -- ^ Not equal (Signed and Unsigned)

        -- unsigned
        | CmpOpUgt    -- ^ Unsigned greater than
        | CmpOpUge    -- ^ Unsigned greater than or equal
        | CmpOpUlt    -- ^ Unsigned less than
        | CmpOpUle    -- ^ Unsigned less than or equal

        -- signed
        | CmpOpSgt    -- ^ Signed greater than
        | CmpOpSge    -- ^ Signed greater than or equal
        | CmpOpSlt    -- ^ Signed less than
        | CmpOpSle    -- ^ Signed less than or equal

        -- float
        | CmpOpFeq    -- ^ Float equal
        | CmpOpFne    -- ^ Float not equal
        | CmpOpFgt    -- ^ Float greater than
        | CmpOpFge    -- ^ Float greater than or equal
        | CmpOpFlt    -- ^ Float less than
        | CmpOpFle    -- ^ Float less than or equal
  deriving (Eq, Show)


instance Pretty CmpOp where
 ppr op
  = case op of
        CmpOpEq       -> text "eq"
        CmpOpNe       -> text "ne"
        CmpOpUgt      -> text "ugt"
        CmpOpUge      -> text "uge"
        CmpOpUlt      -> text "ult"
        CmpOpUle      -> text "ule"
        CmpOpSgt      -> text "sgt"
        CmpOpSge      -> text "sge"
        CmpOpSlt      -> text "slt"
        CmpOpSle      -> text "sle"
        CmpOpFeq      -> text "oeq"
        CmpOpFne      -> text "une"
        CmpOpFgt      -> text "ogt"
        CmpOpFge      -> text "oge"
        CmpOpFlt      -> text "olt"
        CmpOpFle      -> text "ole"


-- CastOp -----------------------------------------------------------------------------------------
-- | Llvm cast operations.
data CastOp
        = CastOpTrunc      -- ^ Integer truncate
        | CastOpZext       -- ^ Integer extend (zero fill)
        | CastOpSext       -- ^ Integer extend (sign fill)
        | CastOpFptrunc    -- ^ Float truncate
        | CastOpFpext      -- ^ Float extend
        | CastOpFptoui     -- ^ Float to unsigned Integer
        | CastOpFptosi     -- ^ Float to signed Integer
        | CastOpUintofp    -- ^ Unsigned Integer to Float
        | CastOpSintofp    -- ^ Signed Int to Float
        | CastOpPtrtoint   -- ^ Pointer to Integer
        | CastOpInttoptr   -- ^ Integer to Pointer
        | CastOpBitcast    -- ^ Cast between types where no bit manipulation is needed
        deriving (Eq, Show)


instance Pretty CastOp where
 ppr op
  = case op of
        CastOpTrunc        -> text "trunc"
        CastOpZext         -> text "zext"
        CastOpSext         -> text "sext"
        CastOpFptrunc      -> text "fptrunc"
        CastOpFpext        -> text "fpext"
        CastOpFptoui       -> text "fptoui"
        CastOpFptosi       -> text "fptosi"
        CastOpUintofp      -> text "uintofp"
        CastOpSintofp      -> text "sintofp"
        CastOpPtrtoint     -> text "ptrtoint"
        CastOpInttoptr     -> text "inttoptr"
        CastOpBitcast      -> text "bitcast"

