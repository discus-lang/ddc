
module DDC.Source.Tetra.Prim.TyConData
        ( kindTyConData
        , tRef)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the kind of a baked-in data constructor.
kindTyConData :: TyConData -> Type Name
kindTyConData tc
 = case tc of
        TyConDataRef    -> kRegion `kFun` kData `kFun` kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Ref` type.
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA   
 = tApps (TCon (TyConBound (UPrim (NameTyConData TyConDataRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData
