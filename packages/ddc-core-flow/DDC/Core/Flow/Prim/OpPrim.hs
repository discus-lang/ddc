
module DDC.Core.Flow.Prim.OpPrim
        ( typePrimCast
        , typePrimArith
        , typePrimVector)
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
typePrimVector :: PrimVector -> Type Name
typePrimVector op
 = case op of
        PrimVectorNeg n -> tForall kData $ \t -> tVec n t `tFun` tVec n t
        PrimVectorAdd n -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVectorSub n -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVectorMul n -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t
        PrimVectorDiv n -> tForall kData $ \t -> tVec n t `tFun` tVec n t `tFun` tVec n t

        PrimVectorRep n 
         -> tForall kData $ \t -> t `tFun` tVec n t

        PrimVectorPack n
         -> tForall kData 
         $  \t -> let Just t' = tFunOfList (replicate n t ++ [tVec n t]) 
                  in  t'

        PrimVectorUnpack n
         -> tForall kData
         $  \t -> tVec n t `tFun` tTupleN  (replicate n t)

