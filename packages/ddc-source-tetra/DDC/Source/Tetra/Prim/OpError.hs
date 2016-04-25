
module DDC.Source.Tetra.Prim.OpError
        ( typeOpError)
where
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp



-- | Take the type of a primitive error function.
typeOpError :: OpError -> Type Name
typeOpError err
 = case err of
        OpErrorDefault    
         -> tForall kData $ \t -> tTextLit `tFun` tNat `tFun` t


