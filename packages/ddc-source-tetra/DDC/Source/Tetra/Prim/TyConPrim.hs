
module DDC.Source.Tetra.Prim.TyConPrim
        ( kindPrimTyCon
        , tBool, tNat, tInt, tWord)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Yield the kind of a type constructor.
kindPrimTyCon :: PrimTyCon -> Kind Name
kindPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConPtr     -> (kRegion `kFun` kData `kFun` kData)
        PrimTyConAddr    -> kData
        PrimTyConBool    -> kData
        PrimTyConNat     -> kData
        PrimTyConInt     -> kData
        PrimTyConWord  _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConTag     -> kData
        PrimTyConVec   _ -> kData `kFun` kData
        PrimTyConString  -> kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- | Primitive `Nat` type.
tNat    ::  Type Name
tNat    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


-- | Primitive `Int` type.
tInt    ::  Type Name
tInt    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- | Primitive `WordN` type of the given width.
tWord   :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)

