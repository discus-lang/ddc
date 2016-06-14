{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Tetra function operators.
module DDC.Source.Tetra.Prim.OpFun
        (typeOpFun)
where
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Prim.TyConTetra
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Compounds


-- | Take the type of a primitive function operator.
typeOpFun :: (Anon l, GTPrim l ~ PrimType) 
          => l -> OpFun -> GType l
typeOpFun l op
 = case op of
        OpFunCurry n
         -> makeTForalls l (replicate (n + 1) KData) $ \ts
         -> let Just tF         = makeTFuns' ts
                Just result     = makeTFuns' (tF : ts)
            in  result

        OpFunApply n
         -> makeTForalls l (replicate (n + 1) KData) $ \ts 
         -> let Just tF         = makeTFuns' ts
                Just result     = makeTFuns' (tF : ts)
            in  result

        OpFunCReify
         -> makeTForalls l [KData, KData] $ \[tA, tB]
         -> (tA ~> tB) ~> TFunValue (tA ~> tB)

        OpFunCCurry n
         -> makeTForalls l (replicate (n + 1) KData) $ \ts 
         -> let tLast : tsFront' = reverse ts
                tsFront = reverse tsFront'
                Just tF = makeTFuns' ts
            in  makeTFuns (TFunValue tF : tsFront) (TCloValue tLast)

        OpFunCExtend n
         -> makeTForalls l (replicate (n + 1) KData) $ \ts
         -> let tLast : tsFront' = reverse ts
                tsFront = reverse tsFront'
                Just tF = makeTFuns' ts
            in  makeTFuns (TCloValue tF : tsFront) (TCloValue tLast)

        OpFunCApply n
         -> makeTForalls l (replicate (n + 1) KData) $ \ts
         -> let tLast : tsFront' = reverse ts
                tsFront = reverse tsFront'
                Just tF = makeTFuns' ts
            in  makeTFuns (TCloValue tF : tsFront) tLast

