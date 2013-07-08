
module DDC.Core.Blue.Prim.TyConPrim 
        ( kindPrimTyCon
        , tVoidU
        , tBoolU
        , tNatU
        , tIntU
        , tWordU)
where
import DDC.Core.Blue.Prim.Base
import DDC.Core.Compounds.Annot
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
        PrimTyConString  -> kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Void#` type.
tVoidU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConVoid) kData) kData)


-- | Primitive `Bool#` type.
tBoolU :: Type Name
tBoolU  = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConBool) kData) kData)


-- | Primitive Nat# type.
tNatU ::  Type Name
tNatU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- | Primitive `Int#` type.
tIntU ::  Type Name
tIntU   = TCon (TyConBound (UPrim (NamePrimTyCon PrimTyConInt) kData) kData)


-- | Primitive `WordN#` type of the given width.
tWordU :: Int -> Type Name
tWordU bits 
        = TCon (TyConBound (UPrim (NamePrimTyCon (PrimTyConWord bits)) kData) kData)

