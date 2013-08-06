
module DDC.Core.Flow.Prim.OpPrim
        ( typePrimCast
        , typePrimArith
        , typePrimVec)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.TyConFlow
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


-- | Take the type of a primitive vector operator.
typePrimVec :: PrimVec -> Type Name
typePrimVec op
 = case op of
        PrimVecNeg n    -> tForall kData $ \t -> tVec n t `tFun` tVec n t
        PrimVecAdd n    -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVecSub n    -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVecMul n    -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVecDiv n    -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t

        PrimVecRep n 
         -> tForall kData $ \t -> t `tFun` tVec n t

        PrimVecPack n
         -> tForall kData 
         $  \t -> let Just t' = tFunOfList (replicate n t ++ [tVec n t]) 
                  in  t'

        PrimVecUnpack n
         -> tForall kData
         $  \t -> tVec n t `tFun` tTupleN  (replicate n t)

        PrimVecGather n
         -> tForall kData
         $  \t -> tVector t `tFun` tVec n tInt `tFun` tVec n t

        PrimVecScatter n
         -> tForall kData
         $  \t -> tVector t `tFun` tVec n tInt `tFun` tVec n t `tFun` tUnit
