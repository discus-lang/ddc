{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive types for Source Tetra language.
module DDC.Source.Tetra.Prim.TyConPrim
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
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Compounds
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
pattern PTrue  = PData (DaConPrim (NameLitBool True)  TBool) []
pattern PFalse = PData (DaConPrim (NameLitBool False) TBool) []


-- Primitives -------------------------------------------------------------------------------------
makeXErrorDefault 
        :: ( GXBoundCon l ~ Name
           , GXPrim l ~ PrimVal
           , GTPrim l ~ PrimType)
        => Text -> Integer -> GExp l
makeXErrorDefault name n
 = makeXApps
        (XPrim (PrimValError OpErrorDefault))
        [ XCon (DaConPrim (NameLitTextLit name) (TBot KData))
        , XCon (DaConPrim (NameLitNat     n)    (TBot KData))]

