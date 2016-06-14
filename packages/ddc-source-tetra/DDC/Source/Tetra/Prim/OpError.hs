{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Tetra.Prim.OpError
        ( typeOpError)
where
import DDC.Source.Tetra.Prim.TyConPrim
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Compounds


-- | Take the type of a primitive error function.
typeOpError l err
 = case err of
        OpErrorDefault    
         -> makeTForall l KData $ \t -> TTextLit ~> TNat ~> t

