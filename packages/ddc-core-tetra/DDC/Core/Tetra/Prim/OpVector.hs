
module DDC.Core.Tetra.Prim.OpVector
        ( readOpVector
        , typeOpVector)
where
import DDC.Core.Tetra.Prim.TyConTetra
import DDC.Core.Tetra.Prim.TyConPrim
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData OpVector where
 rnf !_ = ()


instance Pretty OpVector where
 ppr pv
  = case pv of
        OpVectorAlloc False  -> text "vectorAlloc#"
        OpVectorAlloc True   -> text "vectorAlloc##"

        OpVectorRead  False  -> text "vectorRead#"
        OpVectorRead  True   -> text "vectorRead##"

        OpVectorWrite False  -> text "vectorWrite#"
        OpVectorWrite True   -> text "vectorWrite##"


-- | Read a primitive vector operator.
readOpVector :: String -> Maybe OpVector
readOpVector str
 = case str of
        "vectorAlloc#"  -> Just (OpVectorAlloc False)
        "vectorAlloc##" -> Just (OpVectorRead  True)

        "vectorRead#"   -> Just (OpVectorRead  False)
        "vectorRead##"  -> Just (OpVectorRead  True)

        "vectorWrite#"  -> Just (OpVectorWrite False)
        "vectorWrite##" -> Just (OpVectorWrite True)

        _               -> Nothing


-- | Take the type of a primitive vector operator.
typeOpVector :: OpVector -> Type Name
typeOpVector op
 = case op of
        OpVectorAlloc False
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tNat 
                        `tFun` tSusp (tAlloc tR) (tVector tR tA)

        OpVectorAlloc True
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tUnboxed tNat 
                        `tFun` tSusp (tAlloc tR) (tVector tR tA)


        OpVectorRead  False
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat 
                        `tFun` tSusp (tRead tR) tA

        OpVectorRead  True
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tUnboxed tNat 
                        `tFun` tSusp (tRead tR) (tUnboxed tA)


        OpVectorWrite False
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA  `tFun` tNat `tFun` tA 
                        `tFun` tSusp (tWrite tR) tVoid

        OpVectorWrite True
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tUnboxed tNat `tFun` tUnboxed tA 
                        `tFun` tSusp (tWrite tR) tVoid

