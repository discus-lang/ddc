
-- | Definitions of primitive types for Source Tetra language.
module DDC.Source.Tetra.Prim.TyConPrim
        ( kindPrimTyCon
        , tBool, tNat, tInt, tWord
        , tTextLit)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Yield the kind of a type constructor.
kindPrimTyCon :: PrimTyCon -> Kind Name
kindPrimTyCon tc
 = case tc of
        PrimTyConVoid    -> kData
        PrimTyConBool    -> kData
        PrimTyConNat     -> kData
        PrimTyConInt     -> kData
        PrimTyConSize    -> kData
        PrimTyConWord  _ -> kData
        PrimTyConFloat _ -> kData
        PrimTyConVec   _ -> kData   `kFun` kData
        PrimTyConAddr    -> kData
        PrimTyConPtr     -> kRegion `kFun` kData `kFun` kData
        PrimTyConArray   -> kRegion `kFun` kData `kFun` kData
        PrimTyConTextLit -> kData
        PrimTyConTag     -> kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = TCon (TyConBound (UPrim (NameTyCon PrimTyConBool) kData) kData)


-- | Primitive `Nat` type.
tNat    ::  Type Name
tNat    = TCon (TyConBound (UPrim (NameTyCon PrimTyConNat) kData) kData)


-- | Primitive `Int` type.
tInt    ::  Type Name
tInt    = TCon (TyConBound (UPrim (NameTyCon PrimTyConInt) kData) kData)


-- | Primitive `WordN` type of the given width.
tWord   :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NameTyCon (PrimTyConWord bits)) kData) kData)


-- | Primitive `TextLit` type.
tTextLit  :: Type Name
tTextLit
        = TCon (TyConBound (UPrim (NameTyCon PrimTyConTextLit) kData) kData)


