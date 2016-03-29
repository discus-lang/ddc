
-- Types of primitive Source Tetra vector operators.
module DDC.Source.Tetra.Prim.OpVector
        (typeOpVector)
where
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the type of a primitive vector operator.
typeOpVector :: OpVector -> Type Name
typeOpVector op
 = case op of
        OpVectorAlloc
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tNat `tFun` tSusp (tAlloc tR) (tVector tR tA)

        OpVectorLength
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat

        OpVectorRead
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat `tFun` tSusp (tRead tR) tA

        OpVectorWrite
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat `tFun` tA `tFun` tSusp (tWrite tR) tVoid

