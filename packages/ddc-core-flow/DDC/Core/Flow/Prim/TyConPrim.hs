
module DDC.Core.Flow.Prim.TyConPrim 
        ( kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tWord)
where
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp


-- | Take the kind of a primitive name.
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
        PrimTyConString  -> kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool#` type constructor.
tBool :: Type Name
tBool   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- | Primitive Nat# type constructor.
tNat ::  Type Name
tNat    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat) kData) kData)


-- | Primitive `Int#` type constructor.
tInt ::  Type Name
tInt    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- | Primitive `WordN#` type constructor of the given width.
tWord :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)



