
module DDC.Core.Tetra.Prim.OpCast
        ( readPrimCast
        , typePrimCast)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Type.Exp.Simple
import DDC.Core.Salt.Name       (readPrimCast)


-- | Take the type of a primitive numeric cast operator.
typePrimCast :: PrimCast -> Type Name
typePrimCast op
 = case op of
        PrimCastConvert  
         -> tForalls [kData, kData] $ \[t1, t2] -> t1 `tFun` t2

        PrimCastPromote  
         -> tForalls [kData, kData] $ \[t1, t2] -> t1 `tFun` t2

        PrimCastTruncate 
         -> tForalls [kData, kData] $ \[t1, t2] -> t1 `tFun` t2

