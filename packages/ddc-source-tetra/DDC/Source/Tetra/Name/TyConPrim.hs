
module DDC.Source.Tetra.Name.TyConPrim
        ( tBool, tNat, tInt, tWord
        , tRef)
where
import DDC.Source.Tetra.Name.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimBool) kData) kData)


-- | Primitive `Nat` type.
tNat    ::  Type Name
tNat    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimNat) kData) kData)


-- | Primitive `Int` type.
tInt    ::  Type Name
tInt    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimInt) kData) kData)


-- | Primitive `WordN` type of the given width.
tWord   :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NameTyConPrim (TyConPrimWord bits)) kData) kData)


-- | Primitive `Ref` type.
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA   
 = tApps (TCon (TyConBound (UPrim (NameTyConPrim TyConPrimRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData
