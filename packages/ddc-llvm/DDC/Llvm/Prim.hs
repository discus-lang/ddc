
module DDC.Llvm.Prim
        ( LlvmMachOp    (..)
        , LlvmCmpOp     (..)
        , LlvmCastOp    (..))
where
import DDC.Base.Pretty


-- MachOp -----------------------------------------------------------------------------------------
-- | Llvm binary operators machine operations.
data LlvmMachOp
        = LM_MO_Add     -- ^ add two integers, floating point or vector values.
        | LM_MO_Sub     -- ^ subtract two ...
        | LM_MO_Mul     -- ^ multiply ..
        | LM_MO_UDiv    -- ^ unsigned integer or vector division.
        | LM_MO_SDiv    -- ^ signed integer ..
        | LM_MO_URem    -- ^ unsigned integer or vector remainder (mod)
        | LM_MO_SRem    -- ^ signed ...

        | LM_MO_FAdd    -- ^ add two floating point or vector values.
        | LM_MO_FSub    -- ^ subtract two ...
        | LM_MO_FMul    -- ^ multiply ...
        | LM_MO_FDiv    -- ^ divide ...
        | LM_MO_FRem    -- ^ remainder ...

        | LM_MO_Shl     -- ^ Left shift.
        | LM_MO_LShr    -- ^ Logical shift right
        | LM_MO_AShr    -- ^ Arithmetic shift right.
                        -- The most significant bits of the result will be equal to the sign bit of
                        -- the left operand.


        | LM_MO_And     -- ^ AND bitwise logical operation.
        | LM_MO_Or      -- ^ OR bitwise logical operation.
        | LM_MO_Xor     -- ^ XOR bitwise logical operation.
        deriving (Eq, Show)


instance Pretty LlvmMachOp where
 ppr op
  = case op of
        LM_MO_Add       -> text "add"
        LM_MO_Sub       -> text "sub"
        LM_MO_Mul       -> text "mul"
        LM_MO_UDiv      -> text "udiv"
        LM_MO_SDiv      -> text "sdiv"
        LM_MO_URem      -> text "urem"
        LM_MO_SRem      -> text "srem"
        LM_MO_FAdd      -> text "fadd"
        LM_MO_FSub      -> text "fsub"
        LM_MO_FMul      -> text "fmul"
        LM_MO_FDiv      -> text "fdiv"
        LM_MO_FRem      -> text "frem"
        LM_MO_Shl       -> text "shl"
        LM_MO_LShr      -> text "lshr"
        LM_MO_AShr      -> text "ashr"
        LM_MO_And       -> text "and"
        LM_MO_Or        -> text "or"
        LM_MO_Xor       -> text "xor"


-- CmpOp ------------------------------------------------------------------------------------------
-- | Llvm compare operations.
data LlvmCmpOp
        = LM_CMP_Eq     -- ^ Equal (Signed and Unsigned)
        | LM_CMP_Ne     -- ^ Not equal (Signed and Unsigned)

        -- unsigned
        | LM_CMP_Ugt    -- ^ Unsigned greater than
        | LM_CMP_Uge    -- ^ Unsigned greater than or equal
        | LM_CMP_Ult    -- ^ Unsigned less than
        | LM_CMP_Ule    -- ^ Unsigned less than or equal

        -- signed
        | LM_CMP_Sgt    -- ^ Signed greater than
        | LM_CMP_Sge    -- ^ Signed greater than or equal
        | LM_CMP_Slt    -- ^ Signed less than
        | LM_CMP_Sle    -- ^ Signed less than or equal

        -- float
        | LM_CMP_Feq    -- ^ Float equal
        | LM_CMP_Fne    -- ^ Float not equal
        | LM_CMP_Fgt    -- ^ Float greater than
        | LM_CMP_Fge    -- ^ Float greater than or equal
        | LM_CMP_Flt    -- ^ Float less than
        | LM_CMP_Fle    -- ^ Float less than or equal
  deriving (Eq, Show)


instance Pretty LlvmCmpOp where
 ppr op
  = case op of
        LM_CMP_Eq       -> text "eq"
        LM_CMP_Ne       -> text "ne"

        LM_CMP_Ugt      -> text "ugt"
        LM_CMP_Uge      -> text "uge"
        LM_CMP_Ult      -> text "ult"
        LM_CMP_Ule      -> text "ule"

        LM_CMP_Sgt      -> text "sgt"
        LM_CMP_Sge      -> text "sge"
        LM_CMP_Slt      -> text "slt"
        LM_CMP_Sle      -> text "sle"

        LM_CMP_Feq      -> text "oeq"
        LM_CMP_Fne      -> text "une"
        LM_CMP_Fgt      -> text "ogt"
        LM_CMP_Fge      -> text "oge"
        LM_CMP_Flt      -> text "olt"
        LM_CMP_Fle      -> text "ole"


-- CastOp -----------------------------------------------------------------------------------------
-- | Llvm cast operations.
data LlvmCastOp
        = LM_Trunc      -- ^ Integer truncate
        | LM_Zext       -- ^ Integer extend (zero fill)
        | LM_Sext       -- ^ Integer extend (sign fill)
        | LM_Fptrunc    -- ^ Float truncate
        | LM_Fpext      -- ^ Float extend
        | LM_Fptoui     -- ^ Float to unsigned Integer
        | LM_Fptosi     -- ^ Float to signed Integer
        | LM_Uintofp    -- ^ Unsigned Integer to Float
        | LM_Sintofp    -- ^ Signed Int to Float
        | LM_Ptrtoint   -- ^ Pointer to Integer
        | LM_Inttoptr   -- ^ Integer to Pointer
        | LM_Bitcast    -- ^ Cast between types where no bit manipulation is needed
        deriving (Eq, Show)


instance Pretty LlvmCastOp where
 ppr op
  = case op of
        LM_Trunc        -> text "trunc"
        LM_Zext         -> text "zext"
        LM_Sext         -> text "sext"
        LM_Fptrunc      -> text "fptrunc"
        LM_Fpext        -> text "fpext"
        LM_Fptoui       -> text "fptoui"
        LM_Fptosi       -> text "fptosi"
        LM_Uintofp      -> text "uintofp"
        LM_Sintofp      -> text "sintofp"
        LM_Ptrtoint     -> text "ptrtoint"
        LM_Inttoptr     -> text "inttoptr"
        LM_Bitcast      -> text "bitcast"

