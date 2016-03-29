
-- | Definitions of primitive type constructors for Source Tetra language.
module DDC.Source.Tetra.Prim.TyConTetra
        ( kindPrimTyConTetra
        , readPrimTyConTetra
        , tVector
        , tFunValue
        , tCloValue)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Data.Char
import Data.List
import Control.DeepSeq


instance NFData PrimTyConTetra where
 rnf !_ = ()


instance Pretty PrimTyConTetra where
 ppr tc
  = case tc of
        PrimTyConTetraTuple n   -> text "Tuple" <> int n
        PrimTyConTetraVector    -> text "Vector"
        PrimTyConTetraF         -> text "F#"
        PrimTyConTetraC         -> text "C#"
        PrimTyConTetraU         -> text "U#"


-- | Read the name of a baked-in type constructor.
readPrimTyConTetra :: String -> Maybe PrimTyConTetra
readPrimTyConTetra str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ PrimTyConTetraTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just PrimTyConTetraVector
                "F#"            -> Just PrimTyConTetraF
                "C#"            -> Just PrimTyConTetraC
                "U#"            -> Just PrimTyConTetraU
                _               -> Nothing


-- | Take the kind of a baked-in data constructor.
kindPrimTyConTetra :: PrimTyConTetra -> Type Name
kindPrimTyConTetra tc
 = case tc of
        PrimTyConTetraTuple n   -> foldr kFun kData (replicate n kData)
        PrimTyConTetraVector    -> kRegion `kFun` kData `kFun` kData
        PrimTyConTetraF         -> kData   `kFun` kData
        PrimTyConTetraC         -> kData   `kFun` kData
        PrimTyConTetraU         -> kData   `kFun` kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Vector` type.
tVector ::  Region Name -> Type Name -> Type Name
tVector tR tA   
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra PrimTyConTetraVector) k) k)) 
         [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData


tFunValue :: Type Name -> Type Name
tFunValue tA
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra PrimTyConTetraF) k) k)) [tA]
 where k = kData `kFun` kData


tCloValue :: Type Name -> Type Name
tCloValue tA
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra PrimTyConTetraC) k) k)) [tA]
 where k = kData `kFun` kData

