
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
        PrimArithNeg    -> tForall kData $ \t -> t `tFun` t
        PrimArithAdd    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithSub    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMul    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithDiv    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithMod    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithRem    -> tForall kData $ \t -> t `tFun` t `tFun` t

        -- Bitwise Operators.
        PrimArithShl    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithShr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBAnd   -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBOr    -> tForall kData $ \t -> t `tFun` t `tFun` t
        PrimArithBXOr   -> tForall kData $ \t -> t `tFun` t `tFun` t

        -- Boolean Operators.
        PrimArithAnd    -> tForall kData $ \tb
                        -> tb `tFun` tb `tFun` tb
        
        PrimArithOr     -> tForall kData $ \tb
                        -> tb `tFun` tb `tFun` tb

        -- Comparison Operators.
        --  These are parameterised by the input type, as well as the boolean result, 
        --  so that we can convert between value type and unboxed type representations
        --  in the boxing transform.
        PrimArithEq     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb
        
        PrimArithNeq    -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb
        
        PrimArithGt     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb
        
        PrimArithLt     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb
        
        PrimArithLe     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb
        
        PrimArithGe     -> tForalls [kData, kData] $ \[t, tb]
                        -> t `tFun` t `tFun` tb


