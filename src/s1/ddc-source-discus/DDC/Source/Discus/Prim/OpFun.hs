{-# LANGUAGE TypeFamilies #-}

-- | Types of primitive Source Discus function operators.
module DDC.Source.Discus.Prim.OpFun
        (typeOpFun)
where
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Type.Base
import DDC.Source.Discus.Exp.Term.Compounds


-- | Take the type of a primitive function operator.
typeOpFun :: l -> OpFun -> GType l
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

        OpFunReify
         -> makeTForalls l [KData, KData] $ \[tA, tB]
         -> (tA ~> tB) ~> TFunValue (tA ~> tB)

