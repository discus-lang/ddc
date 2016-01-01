
-- | Definitions of primitive type constructors for Source Tetra language.
module DDC.Source.Tetra.Prim.TyConTetra
        ( kindTyConTetra
        , readTyConTetra
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


instance NFData TyConTetra where
 rnf !_ = ()


instance Pretty TyConTetra where
 ppr tc
  = case tc of
        TyConTetraTuple n       -> text "Tuple" <> int n
        TyConTetraF             -> text "F#"
        TyConTetraC             -> text "C#"
        TyConTetraU             -> text "U#"


-- | Read the name of a baked-in type constructor.
readTyConTetra :: String -> Maybe TyConTetra
readTyConTetra str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConTetraTuple arity

        | otherwise
        = case str of
                "F#"            -> Just TyConTetraF
                "C#"            -> Just TyConTetraC
                "U#"            -> Just TyConTetraU
                _               -> Nothing


-- | Take the kind of a baked-in data constructor.
kindTyConTetra :: TyConTetra -> Type Name
kindTyConTetra tc
 = case tc of
        TyConTetraTuple n -> foldr kFun kData (replicate n kData)
        TyConTetraF       -> kData `kFun` kData
        TyConTetraC       -> kData `kFun` kData
        TyConTetraU       -> kData `kFun` kData


-- Compounds ------------------------------------------------------------------
tFunValue :: Type Name -> Type Name
tFunValue tA
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra TyConTetraF) k) k)) [tA]
 where k = kData `kFun` kData


tCloValue :: Type Name -> Type Name
tCloValue tA
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra TyConTetraC) k) k)) [tA]
 where k = kData `kFun` kData
