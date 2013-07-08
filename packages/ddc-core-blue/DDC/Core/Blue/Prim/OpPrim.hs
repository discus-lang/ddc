
module DDC.Core.Blue.Prim.OpPrim
        ( readOpPrimArith
        , typeOpPrimArith)
where
import DDC.Core.Blue.Prim.TyConPrim
import DDC.Core.Blue.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List


instance NFData OpPrimArith

instance Pretty OpPrimArith where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) opPrimArithNames
    in  (text n)


-- | Read a primitive operator.
readOpPrimArith :: String -> Maybe OpPrimArith
readOpPrimArith str
  =  case find (\(_, n) -> str == n) opPrimArithNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
opPrimArithNames :: [(OpPrimArith, String)]
opPrimArithNames
 =      [ (OpPrimArithNeg,        "neg#")
        , (OpPrimArithAdd,        "add#")
        , (OpPrimArithSub,        "sub#")
        , (OpPrimArithMul,        "mul#")
        , (OpPrimArithDiv,        "div#")
        , (OpPrimArithRem,        "rem#")
        , (OpPrimArithMod,        "mod#")
        , (OpPrimArithEq ,        "eq#" )
        , (OpPrimArithNeq,        "neq#")
        , (OpPrimArithGt ,        "gt#" )
        , (OpPrimArithGe ,        "ge#" )
        , (OpPrimArithLt ,        "lt#" )
        , (OpPrimArithLe ,        "le#" )
        , (OpPrimArithAnd,        "and#")
        , (OpPrimArithOr ,        "or#" ) 
        , (OpPrimArithShl,        "shl#")
        , (OpPrimArithShr,        "shr#")
        , (OpPrimArithBAnd,       "band#")
        , (OpPrimArithBOr,        "bor#")
        , (OpPrimArithBXOr,       "bxor#") ]


-- | Take the type of a primitive arithmetic operator.
typeOpPrimArith :: OpPrimArith -> Type Name
typeOpPrimArith op
 = case op of
        -- Numeric
        OpPrimArithNeg  -> tForall kData $ \t -> t `tFunPE` t
        OpPrimArithAdd  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithSub  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithMul  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithDiv  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithMod  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithRem  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Comparison
        OpPrimArithEq   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        OpPrimArithNeq  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        OpPrimArithGt   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        OpPrimArithLt   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        OpPrimArithLe   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool
        OpPrimArithGe   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBool

        -- Boolean
        OpPrimArithAnd  -> tBool `tFunPE` tBool `tFunPE` tBool
        OpPrimArithOr   -> tBool `tFunPE` tBool `tFunPE` tBool

        -- Bitwise
        OpPrimArithShl  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithShr  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithBAnd -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithBOr  -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        OpPrimArithBXOr -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

