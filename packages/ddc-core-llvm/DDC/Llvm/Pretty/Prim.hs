
module DDC.Llvm.Pretty.Prim where
import DDC.Llvm.Syntax.Prim
import DDC.Base.Pretty

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






