
-- | Types of primitive Source Tetra function operators.
module DDC.Source.Tetra.Prim.OpFun
        (typeOpFun)
where
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the type of a primitive function operator.
typeOpFun :: OpFun -> Type Name
typeOpFun op
 = case op of
        OpFunCurry n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let Just tF          = tFunOfList ts
                    Just result      = tFunOfList (tF : ts)
                in  result

        OpFunApply n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let Just tF          = tFunOfList ts
                    Just result      = tFunOfList (tF : ts)
                in  result

        OpFunCReify
         -> tForalls [kData, kData]
         $  \[tA, tB]  -> (tA `tFun` tB) `tFun` tFunValue (tA `tFun` tB)

        OpFunCCurry n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result         
                        = tFunOfList 
                                ( tFunValue tF
                                : tsFront ++ [tCloValue tLast])
                in result

        OpFunCExtend n
         -> tForalls (replicate (n + 1) kData)
         $  \ts -> 
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result
                        = tFunOfList
                                ( tCloValue tF
                                : tsFront ++ [tCloValue tLast])
                in result

        OpFunCApply n
         -> tForalls (replicate (n + 1) kData)
         $  \ts ->
                let tLast : tsFront' = reverse ts
                    tsFront          = reverse tsFront'
                    Just tF          = tFunOfList ts
                    Just result
                        = tFunOfList
                                ( tCloValue tF
                                : tsFront ++ [tLast])
                in result

