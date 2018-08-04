{-# LANGUAGE OverloadedStrings #-}

module DDC.Llvm.Write.Prim where
import DDC.Llvm.Syntax.Prim
import DDC.Llvm.Write.Base


instance Write Config Op where
 write o op
  = case op of
        OpAdd           -> text o "add"
        OpSub           -> text o "sub"
        OpMul           -> text o "mul"
        OpUDiv          -> text o "udiv"
        OpSDiv          -> text o "sdiv"
        OpURem          -> text o "urem"
        OpSRem          -> text o "srem"
        OpFAdd          -> text o "fadd"
        OpFSub          -> text o "fsub"
        OpFMul          -> text o "fmul"
        OpFDiv          -> text o "fdiv"
        OpFRem          -> text o "frem"
        OpShl           -> text o "shl"
        OpLShr          -> text o "lshr"
        OpAShr          -> text o "ashr"
        OpAnd           -> text o "and"
        OpOr            -> text o "or"
        OpXor           -> text o "xor"


instance Write Config ICond where
 write o pp
  = case pp of
        ICondEq         -> text o "eq"
        ICondNe         -> text o "ne"
        ICondUgt        -> text o "ugt"
        ICondUge        -> text o "uge"
        ICondUlt        -> text o "ult"
        ICondUle        -> text o "ule"
        ICondSgt        -> text o "sgt"
        ICondSge        -> text o "sge"
        ICondSlt        -> text o "slt"
        ICondSle        -> text o "sle"


instance Write Config FCond where
 write o pp
  = case pp of
        FCondFalse      -> text o "false"
        FCondOeq        -> text o "oeq"
        FCondOgt        -> text o "ogt"
        FCondOge        -> text o "oge"
        FCondOlt        -> text o "olt"
        FCondOle        -> text o "ole"
        FCondOne        -> text o "one"
        FCondOrd        -> text o "ord"
        FCondUeq        -> text o "ueq"
        FCondUgt        -> text o "ugt"
        FCondUge        -> text o "uge"
        FCondUlt        -> text o "ult"
        FCondUle        -> text o "ule"
        FCondUne        -> text o "une"
        FCondUno        -> text o "uno"
        FCondTrue       -> text o "true"


instance Write Config Conv where
 write o pp
  = case pp of
        ConvTrunc       -> text o "trunc"
        ConvZext        -> text o "zext"
        ConvSext        -> text o "sext"
        ConvFptrunc     -> text o "fptrunc"
        ConvFpext       -> text o "fpext"
        ConvFptoui      -> text o "fptoui"
        ConvFptosi      -> text o "fptosi"
        ConvUintofp     -> text o "uitofp"
        ConvSintofp     -> text o "sitofp"
        ConvPtrtoint    -> text o "ptrtoint"
        ConvInttoptr    -> text o "inttoptr"
        ConvBitcast     -> text o "bitcast"

