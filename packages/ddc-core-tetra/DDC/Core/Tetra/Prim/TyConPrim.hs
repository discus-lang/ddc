
module DDC.Core.Tetra.Prim.TyConPrim
        ( PrimTyCon     (..)
        , pprPrimTyConStem
        , readPrimTyCon,        readPrimTyConStem
        , kindPrimTyCon
        , tVoid
        , tBool
        , tNat, tInt, tSize, tWord, tFloat
        , tPtr
        , tTextLit)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Compounds
import DDC.Core.Exp.Simple.Exp
import DDC.Core.Salt.Name
        ( pprPrimTyConStem
        , readPrimTyCon, readPrimTyConStem)


-- | Yield the kind of a type constructor.
kindPrimTyCon :: PrimTyCon -> Kind Name
kindPrimTyCon tc
 = case tc of
        PrimTyConVoid      -> kData
        PrimTyConBool      -> kData
        PrimTyConNat       -> kData
        PrimTyConInt       -> kData
        PrimTyConSize      -> kData
        PrimTyConWord{}    -> kData
        PrimTyConFloat{}   -> kData
        PrimTyConVec{}     -> kData   `kFun` kData
        PrimTyConAddr{}    -> kData
        PrimTyConPtr{}     -> kRegion `kFun` kData `kFun` kData
        PrimTyConTextLit{} -> kData
        PrimTyConTag{}     -> kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Void` type.
tVoid   :: Type Name
tVoid   = tConPrim PrimTyConVoid


-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = tConPrim PrimTyConBool


-- | Primitive `Nat` type.
tNat    :: Type Name
tNat    = tConPrim PrimTyConNat


-- | Primitive `Int` type.
tInt    :: Type Name
tInt    = tConPrim PrimTyConInt


-- | Primitive `WordN` type of the given width.
tWord   :: Int -> Type Name
tWord bits = tConPrim (PrimTyConWord bits)


-- | Primitive `Size` type.
tSize   :: Type Name
tSize   = tConPrim PrimTyConSize


-- | Primitive `FloatN` type of the given width.
tFloat  :: Int -> Type Name
tFloat bits = tConPrim (PrimTyConFloat bits)


-- | Primitive `Ptr` type with given region and data type
tPtr   :: Type Name -> Type Name -> Type Name
tPtr r a
         = tConPrim PrimTyConPtr `TApp` r `TApp` a


-- | The text literal type.
tTextLit :: Type Name
tTextLit = tConPrim PrimTyConTextLit


-- | Yield the type for a primtiive type constructor.
tConPrim :: PrimTyCon -> Type Name
tConPrim tc
 = let k = kindPrimTyCon tc
   in      TCon (TyConBound (UPrim (NamePrimTyCon tc) k) k)


