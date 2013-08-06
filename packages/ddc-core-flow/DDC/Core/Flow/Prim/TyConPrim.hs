
module DDC.Core.Flow.Prim.TyConPrim 
        ( kindPrimTyCon
        , tVoid
        , tBool
        , tNat
        , tInt
        , tFloat
        , tWord
        , tVec)
where
import DDC.Core.Flow.Prim.Base
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple


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
-- | Primitive `Void#` type.
tVoid   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid) kData) kData)


-- | Primitive `Bool#` type.
tBool :: Type Name
tBool   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- | Primitive Nat# type.
tNat ::  Type Name
tNat    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConNat)  kData) kData)


-- | Primitive `Int#` type.
tInt ::  Type Name
tInt    = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt)  kData) kData)


-- | Primitive `FloatN#` type of the given width.
tFloat :: Int -> Type Name
tFloat bits
        = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConFloat bits)) kData) kData)


-- | Primitive `WordN#` type of the given width.
tWord :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)


-- | Primitive @VecN# a@.
tVec  :: Int -> Type Name -> Type Name
tVec n tA = TApp (tConPrimTyCon (PrimTyConVec n)) tA


-- Utils ----------------------------------------------------------------------
tConPrimTyCon :: PrimTyCon -> Type Name
tConPrimTyCon tcp
 = let  k       = kindPrimTyCon tcp
        u       = UPrim (NamePrimTyCon tcp) k
        tc      = TyConBound u k
   in   TCon tc
