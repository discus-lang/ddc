{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive types for Source Discus language.
module DDC.Source.Discus.Prim.TyConPrim
        ( kindPrimTyCon
        , pattern TVoid
        , pattern TBool
        , pattern TNat
        , pattern TInt
        , pattern TSize
        , pattern TWord
        , pattern TFloat
        , pattern TTextLit

        , pattern PTrue
        , pattern PFalse

        , makeXErrorDefault)
where
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Prim.TyCon
import DDC.Source.Discus.Exp.Generic
import DDC.Source.Discus.Exp.Bind
import DDC.Source.Discus.Exp.Compounds
import Data.Text                        (Text)


-- | Yield the kind of a type constructor.
kindPrimTyCon
        :: (PrimType ~ GTPrim l)
        => PrimTyCon -> GType l

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


-- Compounds --------------------------------------------------------------------------------------
-- | Primitive `Bool` type.
pattern TBool           = TCon (TyConPrim (PrimTypeTyCon PrimTyConBool))

-- | Primitive `Nat` type.
pattern TNat            = TCon (TyConPrim (PrimTypeTyCon PrimTyConNat))

-- | Primitive `Int` type.
pattern TInt            = TCon (TyConPrim (PrimTypeTyCon PrimTyConInt))

-- | Primitive `Size` type.
pattern TSize           = TCon (TyConPrim (PrimTypeTyCon PrimTyConSize) )

-- | Primitive `WordN` type of the given width.
pattern TWord bits      = TCon (TyConPrim (PrimTypeTyCon (PrimTyConWord bits)))

-- | Primitive `FloatN` type of the given width.
pattern TFloat bits     = TCon (TyConPrim (PrimTypeTyCon (PrimTyConFloat bits)))

-- | Primitive `TextLit` type.
pattern TTextLit        = TCon (TyConPrim (PrimTypeTyCon PrimTyConTextLit))


-- Patterns ---------------------------------------------------------------------------------------
pattern PTrue  = PData (DaConPrim (DaConBoundLit (PrimLitBool True))  TBool) []
pattern PFalse = PData (DaConPrim (DaConBoundLit (PrimLitBool False)) TBool) []


-- Primitives -------------------------------------------------------------------------------------
makeXErrorDefault
        :: ( GXBoundCon l ~ DaConBound
           , GXFrag l     ~ PrimVal
           , GTPrim l     ~ PrimType)
        => Text -> Integer -> GExp l
makeXErrorDefault name n
 = makeXApps
        (XFrag (PrimValError OpErrorDefault))
        [ RTerm $ XCon (DaConPrim (DaConBoundLit (PrimLitTextLit name)) (TBot KData))
        , RTerm $ XCon (DaConPrim (DaConBoundLit (PrimLitNat     n))    (TBot KData))]

