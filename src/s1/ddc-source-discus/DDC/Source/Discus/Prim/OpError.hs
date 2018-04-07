{-# LANGUAGE TypeFamilies #-}
module DDC.Source.Discus.Prim.OpError
        ( typeOpError)
where
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Type.Base
import DDC.Source.Discus.Exp.Term.Compounds


-- | Take the type of a primitive error function.
typeOpError l err
 = case err of
        OpErrorDefault
         -> makeTForall l KData $ \t -> TTextLit ~> TNat ~> t

