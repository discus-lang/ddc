
module DDC.Core.Tetra.Prim.TyConPrim
        ( PrimTyCon     (..)
        , readPrimTyCon
        , kindPrimTyCon
        , tBool
        , tNat
        , tInt
        , tWord)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Salt.Name.PrimTyCon
import DDC.Core.Compounds.Annot
import DDC.Core.Exp.Simple


-- | Yield the kind of a type constructor.
kindPrimTyCon :: PrimTyCon -> Kind Name
kindPrimTyCon tc
 = case tc of
        PrimTyConVoid     -> kData
        PrimTyConBool     -> kData
        PrimTyConNat      -> kData
        PrimTyConInt      -> kData
        PrimTyConWord{}   -> kData
        PrimTyConFloat{}  -> kData
        PrimTyConVec{}    -> kData   `kFun` kData
        PrimTyConAddr{}   -> kData
        PrimTyConPtr{}    -> kRegion `kFun` kData `kFun` kData
        PrimTyConTag{}    -> kData
        PrimTyConString{} -> kData


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

