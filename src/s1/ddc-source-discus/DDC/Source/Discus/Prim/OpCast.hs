{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Discus arithmetic operators.
module DDC.Source.Discus.Prim.OpCast
        (typePrimCast)
where
import DDC.Source.Discus.Prim.TyCon
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Generic
import DDC.Source.Discus.Exp.Compounds


-- | Take the type of a primitive arithmetic operator.
typePrimCast
        :: (Anon l, GTPrim l ~ PrimType)
        => l -> PrimCast -> GType l
typePrimCast l op
 = case op of
        PrimCastConvert
         -> makeTForalls l [KData, KData]
         $  \[t1, t2] -> t1 ~> t2

        PrimCastPromote
         -> makeTForalls l [KData, KData]
         $  \[t1, t2] -> t1 ~> t2

        PrimCastTruncate
         -> makeTForalls l [KData, KData]
         $  \[t1, t2] -> t1 ~> t2
