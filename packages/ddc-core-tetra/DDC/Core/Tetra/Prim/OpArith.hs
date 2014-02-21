
module DDC.Core.Tetra.Prim.OpArith
        ( readPrimArith
        , typePrimArith)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Salt.Name.PrimArith
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the type of a primitive arithmetic operator.
typePrimArith :: PrimArith -> Type Name
typePrimArith op
 = case op of
        -- Arithmetic Operators.
        --  Parameterised by the type they work on.
        PrimArithNeg    -> tForall kData $ \t -> t `tFunPE` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithSub    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithMul    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithMod    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithRem    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Bitwise Operators.
        PrimArithShl    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithShr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFunPE` t `tFunPE` t

        -- Boolean Operators.
        PrimArithAnd    -> tForall kData $ \tb
                        -> tb `tFunPE` tb `tFunPE` tb
        
        PrimArithOr     -> tForall kData $ \tb
                        -> tb `tFunPE` tb `tFunPE` tb

        -- Comparison Operators.
        --  These are parameterised by the input type, as well as the boolean result, 
        --  so that we can convert between value type and unboxed type representations
        --  in the boxing transform.
        PrimArithEq     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb
        
        PrimArithNeq    -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb
        
        PrimArithGt     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb
        
        PrimArithLt     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb
        
        PrimArithLe     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb
        
        PrimArithGe     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFunPE` t `tFunPE` tb


