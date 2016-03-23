
-- Types of primitive Source Tetra vector operators.
module DDC.Source.Tetra.Prim.OpVector
        ( readOpVector
        , typeOpVector)
where
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpVector where
 rnf !_ = ()


instance Pretty OpVector where
 ppr pv
  = case pv of
        OpVectorAlloc   -> text "vectorAlloc#"
        OpVectorRead    -> text "vectorRead#"
        OpVectorWrite   -> text "vectorWrite#"


-- | Read a primitive vector operator.
readOpVector :: String -> Maybe OpVector
readOpVector str
 = case str of
        "vectorAlloc#"  -> Just OpVectorAlloc
        "vectorRead#"   -> Just OpVectorRead
        "vectorWrite#"  -> Just OpVectorWrite
        _               -> Nothing


-- | Take the type of a primitive vector operator.
typeOpVector :: OpVector -> Type Name
typeOpVector op
 = case op of
        OpVectorAlloc
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tNat `tFun` tSusp (tAlloc tR) (tVector tR tA)

        OpVectorRead
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat `tFun` tSusp (tRead tR) tA

        OpVectorWrite
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat `tFun` tA `tFun` tSusp (tWrite tR) tVoid

