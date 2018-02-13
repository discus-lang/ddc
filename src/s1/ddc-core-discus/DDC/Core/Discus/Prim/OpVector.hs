
module DDC.Core.Discus.Prim.OpVector
        ( readOpVectorFlag
        , typeOpVectorFlag)
where
import DDC.Core.Discus.Prim.TyConDiscus
import DDC.Core.Discus.Prim.TyConPrim
import DDC.Core.Discus.Prim.Base
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import Control.DeepSeq


instance NFData OpVector where
 rnf !_ = ()


instance Pretty OpVector where
 ppr pv
  = case pv of
        OpVectorAlloc   -> text "vectorAlloc#"
        OpVectorLength  -> text "vectorLength#"
        OpVectorRead    -> text "vectorRead#"
        OpVectorWrite   -> text "vectorWrite#"


-- | Read a primitive vector operator,
--   along with the flag that indicates whether this is the
--   boxed or unboxed version.
readOpVectorFlag :: String -> Maybe (OpVector, Bool)
readOpVectorFlag str
 = case str of
        "vectorAlloc#"   -> Just (OpVectorAlloc,  False)
        "vectorAlloc##"  -> Just (OpVectorAlloc,  True)

        "vectorLength#"  -> Just (OpVectorLength, False)
        "vectorLength##" -> Just (OpVectorLength, True)

        "vectorRead#"    -> Just (OpVectorRead,   False)
        "vectorRead##"   -> Just (OpVectorRead,   True)

        "vectorWrite#"   -> Just (OpVectorWrite,  False)
        "vectorWrite##"  -> Just (OpVectorWrite,  True)

        _                -> Nothing


-- | Take the type of a primitive vector operator.
typeOpVectorFlag :: OpVector -> Bool -> Type Name

typeOpVectorFlag op False
 = case op of
        OpVectorAlloc
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tNat
                        `tFun` tSusp (tAlloc tR) (tVector tR tA)

        OpVectorLength
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA
                        `tFun` tNat

        OpVectorRead
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tNat
                        `tFun` tSusp (tRead tR) tA

        OpVectorWrite
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA  `tFun` tNat `tFun` tA
                        `tFun` tSusp (tWrite tR) tVoid

typeOpVectorFlag op True
 = case op of
        OpVectorAlloc
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tUnboxed tNat
                        `tFun` tSusp (tAlloc tR) (tVector tR tA)

        OpVectorLength
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA
                        `tFun` tUnboxed tNat

        OpVectorRead
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tUnboxed tNat
                        `tFun` tSusp (tRead tR) (tUnboxed tA)

        OpVectorWrite
         -> tForalls [kRegion, kData]
         $  \[tR, tA] -> tVector tR tA `tFun` tUnboxed tNat `tFun` tUnboxed tA
                        `tFun` tSusp (tWrite tR) tVoid


