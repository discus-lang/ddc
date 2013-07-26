
module DDC.Core.Flow.Prim.OpPrim
        ( typePrimCast
        , typePrimArith)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple


-- | Take the type of a primitive cast.
typePrimCast :: PrimCast -> Type Name
typePrimCast cc
 = case cc of
        PrimCastPromote
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1

        PrimCastTruncate
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1


-- | Take the type of a primitive arithmetic operator.
typePrimArith :: PrimArith -> Type Name
typePrimArith op
 = case op of
        -- Numeric
        PrimArithNeg    -> tForall kData $ \t -> t `tFun` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithSub    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMul    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMod    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithRem    -> tForall kData $ \t -> t `tFun` t `tFun` t

        -- Comparison
        PrimArithEq     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithNeq    -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithGt     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithLt     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithLe     -> tForall kData $ \t -> t `tFun` t `tFun` tBool
        PrimArithGe     -> tForall kData $ \t -> t `tFun` t `tFun` tBool

        -- Boolean
        PrimArithAnd    -> tBool `tFun` tBool `tFun` tBool
        PrimArithOr     -> tBool `tFun` tBool `tFun` tBool

        -- Bitwise
        PrimArithShl    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithShr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFun` t `tFun` t
