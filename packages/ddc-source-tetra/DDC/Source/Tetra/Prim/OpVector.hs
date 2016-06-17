{-# LANGUAGE TypeFamilies #-}
-- Types of primitive Source Tetra vector operators.
module DDC.Source.Tetra.Prim.OpVector
        (typeOpVector)
where
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Compounds


-- | Take the type of a primitive vector operator.
typeOpVector :: forall l. (Anon l, GTPrim l ~ PrimType) => l -> OpVector -> GType l
typeOpVector l op
 = case op of
        OpVectorAlloc
         -> makeTForalls l [KRegion, KData] $ \[tR, tA]
         -> TSusp (TAlloc tR) (TVector tR tA)

        OpVectorLength
         -> makeTForalls l [KRegion, KData] $ \[tR, tA]
         -> TVector tR tA ~> TNat

        OpVectorRead
         -> makeTForalls l [KRegion, KData] $ \[tR, tA]
         -> TVector tR tA ~> TNat ~> TSusp (TRead tR) tA

        OpVectorWrite
         -> makeTForalls l [KRegion, KData] $ \[tR, tA]
         -> TVector tR tA ~> TNat ~> tA ~> TSusp (TWrite tR) TVoid

