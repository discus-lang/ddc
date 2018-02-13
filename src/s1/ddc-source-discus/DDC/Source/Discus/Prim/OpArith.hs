{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Discus arithmetic operators.
module DDC.Source.Discus.Prim.OpArith
        (typePrimArith)
where
import DDC.Source.Discus.Prim.TyCon
import DDC.Source.Discus.Prim.TyConPrim
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Generic
import DDC.Source.Discus.Exp.Compounds


-- | Take the type of a primitive arithmetic operator.
typePrimArith
        :: (Anon l, GTPrim l ~ PrimType)
        => l -> PrimArith -> GType l
typePrimArith l op
 = case op of
        -- Numeric
        PrimArithNeg  -> makeTForall l KData $ \t -> t ~> t
        PrimArithAdd  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithSub  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithMul  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithDiv  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithMod  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithRem  -> makeTForall l KData $ \t -> t ~> t ~> t

        -- Comparison
        PrimArithEq   -> makeTForall l KData $ \t -> t ~> t ~> TBool
        PrimArithNeq  -> makeTForall l KData $ \t -> t ~> t ~> TBool
        PrimArithGt   -> makeTForall l KData $ \t -> t ~> t ~> TBool
        PrimArithLt   -> makeTForall l KData $ \t -> t ~> t ~> TBool
        PrimArithLe   -> makeTForall l KData $ \t -> t ~> t ~> TBool
        PrimArithGe   -> makeTForall l KData $ \t -> t ~> t ~> TBool

        -- Boolean
        PrimArithAnd  -> TBool ~> TBool ~> TBool
        PrimArithOr   -> TBool ~> TBool ~> TBool

        -- Bitwise
        PrimArithShl  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithShr  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithBAnd -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithBOr  -> makeTForall l KData $ \t -> t ~> t ~> t
        PrimArithBXOr -> makeTForall l KData $ \t -> t ~> t ~> t

