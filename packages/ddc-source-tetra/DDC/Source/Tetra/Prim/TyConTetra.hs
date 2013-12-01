
module DDC.Source.Tetra.Prim.TyConTetra
        ( kindTyConTetra
        , tRef)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp


-- | Take the kind of a baked-in data constructor.
kindTyConTetra :: TyConTetra -> Type Name
kindTyConTetra tc
 = case tc of
        TyConTetraRef     -> kRegion `kFun` kData `kFun` kData
        TyConTetraTuple n -> foldr kFun kData (replicate n kData)


-- Compounds ------------------------------------------------------------------
-- | Primitive `Ref` type.
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA   
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra TyConTetraRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData
