{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive types for Source Discus language.
module DDC.Source.Discus.Exp.Type.Prim.TyConPrim
        ( kindPrimTyCon

        , pattern PTrue
        , pattern PFalse

        , makeXErrorDefault)
where
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Type.Base
import DDC.Source.Discus.Exp.Term.Base
import DDC.Source.Discus.Exp.Term.Compounds
import Data.Text                        (Text)


---------------------------------------------------------------------------------------------------
-- | Yield the kind of a type constructor.
kindPrimTyCon :: PrimTyCon -> GType l
kindPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> KData
        PrimTyConBool    -> KData
        PrimTyConNat     -> KData
        PrimTyConInt     -> KData
        PrimTyConSize    -> KData
        PrimTyConWord  _ -> KData
        PrimTyConFloat _ -> KData
        PrimTyConVec   _ -> KData   ~> KData
        PrimTyConAddr    -> KData
        PrimTyConPtr     -> KRegion ~> KData ~> KData
        PrimTyConTextLit -> KData
        PrimTyConTag     -> KData


-- Patterns ---------------------------------------------------------------------------------------
pattern PTrue  = PData (DaConPrim (DaConBoundLit (PrimLitBool True))  TBool) []
pattern PFalse = PData (DaConPrim (DaConBoundLit (PrimLitBool False)) TBool) []


-- Primitives -------------------------------------------------------------------------------------
makeXErrorDefault
        :: (GXFrag l     ~ PrimVal)
        => Text -> Integer -> GExp l
makeXErrorDefault name n
 = makeXApps
        (XFrag (PrimValError OpErrorDefault))
        [ RTerm $ XCon (DaConPrim (DaConBoundLit (PrimLitTextLit name)) (TBot KData))
        , RTerm $ XCon (DaConPrim (DaConBoundLit (PrimLitNat     n))    (TBot KData))]

