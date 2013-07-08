
module DDC.Core.Blue.Prim.OpPrim
        ( typePrimCast
        , typePrimArith)
where
import DDC.Core.Blue.Prim.TyConPrim
import DDC.Core.Blue.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the type of a primitive cast.
typePrimCast :: PrimCast -> Type Name
typePrimCast cc
 = case cc of
        PrimCastPromote
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1

        PrimCastTruncate
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFunPE` t1


-- | Take the type of a primitive arithmetic operator.
typePrimArith :: PrimArith -> Type Name
typePrimArith op
 = case op of
        -- Numeric
        PrimArithNeg    -> tForall kData $ \t -> t `tFunPE` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithSub    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithMul    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithMod    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithRem    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Comparison
        PrimArithEq     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithNeq    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithGt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithLt     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithLe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU
        PrimArithGe     -> tForall kData $ \t -> t `tFunPE` t `tFunPE` tBoolU

        -- Boolean
        PrimArithAnd    -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU
        PrimArithOr     -> tBoolU `tFunPE` tBoolU `tFunPE` tBoolU

        -- Bitwise
        PrimArithShl    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithShr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
