{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Tetra arithmetic operators.
module DDC.Source.Tetra.Prim.OpCast
        (typePrimCast)
where
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Compounds


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
