{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Discus function operators.
module DDC.Source.Discus.Prim.OpFun
        (typeOpFun)
where
import DDC.Source.Discus.Prim.TyCon
import DDC.Source.Discus.Prim.TyConDiscus
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Generic
import DDC.Source.Discus.Exp.Compounds


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

