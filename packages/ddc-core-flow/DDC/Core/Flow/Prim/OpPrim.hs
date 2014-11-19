
module DDC.Core.Flow.Prim.OpPrim
        ( typePrimCast
        , typePrimArith
        , typePrimVec

          -- * Compounds
        , xvRep
        , xvProj
        , xvGather
        , xvScatter)
where
import DDC.Core.Flow.Prim.TyConPrim
import DDC.Core.Flow.Prim.TyConFlow
import DDC.Core.Flow.Prim.KiConFlow
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple


-- | Take the type of a primitive cast.
typePrimCast :: PrimCast -> Type Name
typePrimCast cc
 = case cc of
        PrimCastConvert
         -> tForalls [kData, kData] $ \[t1, t2] -> t2 `tFun` t1

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

        PrimVecProj n _
         -> tForall kData
         $  \t -> tVec n t `tFun` t

        PrimVecGather n
         -> tForalls [kRate, kData]
         $  \[k, t] -> tRateVec k t `tFun` tVec n tNat `tFun` tVec n t

        PrimVecScatter n
         -> tForall kData
         $  \t -> tVector t `tFun` tVec n tNat `tFun` tVec n t `tFun` tUnit


-- Compounds ------------------------------------------------------------------
xvRep   :: Int -> Type Name -> Exp () Name -> Exp () Name
xvRep c tA xZ
 = xApps (xVarPrimVec (PrimVecRep c))   
         [XType tA, xZ]

xvProj   :: Int -> Int -> Type Name -> Exp () Name -> Exp () Name
xvProj n i tA xV
 = xApps (xVarPrimVec (PrimVecProj n i))
         [XType tA, xV]

xvGather  :: Int -> Type Name -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name
xvGather c tK tA xVec xIxs
 = xApps (xVarPrimVec (PrimVecGather c))
         [XType tK, XType tA, xVec, xIxs]


xvScatter :: Int -> Type Name -> Exp () Name -> Exp () Name -> Exp () Name -> Exp () Name
xvScatter c tA xVec xIxs xElems
 = xApps (xVarPrimVec (PrimVecScatter c))
         [XType tA, xVec, xIxs, xElems]


-- Utils -----------------------------------------------------------------------
xVarPrimVec :: PrimVec -> Exp () Name
xVarPrimVec op
        = XVar  (UPrim (NamePrimVec op) (typePrimVec op))

