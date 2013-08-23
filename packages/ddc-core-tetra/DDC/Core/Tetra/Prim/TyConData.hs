
module DDC.Core.Tetra.Prim.TyConData
        ( kindTyConData
        , readTyConData
        , tRef)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Compounds.Annot
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq


instance NFData TyConData

instance Pretty TyConData where
 ppr tc
  = case tc of
        TyConDataRef    -> text "Ref#"


-- | Read the name of a baked-in data constructor.
readTyConData :: String -> Maybe TyConData
readTyConData str
 = case str of
        "Ref#"          -> Just TyConDataRef
        _               -> Nothing


-- | Take the kind of a baked-in data constructor.
kindTyConData :: TyConData -> Type Name
kindTyConData tc
 = case tc of
        TyConDataRef    -> kRegion `kFun` kData `kFun` kData


-- Compounds ------------------------------------------------------------------
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA
 = tApps (TCon (TyConBound (UPrim (NameTyConData TyConDataRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData
