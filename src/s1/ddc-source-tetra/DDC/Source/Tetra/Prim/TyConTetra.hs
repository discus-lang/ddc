{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive type constructors for Source Tetra language.
module DDC.Source.Tetra.Prim.TyConTetra
        ( kindPrimTyConTetra
        , readPrimTyConTetra
        , pattern TVector
        , pattern TFunValue
        , pattern TCloValue)
where
import DDC.Source.Tetra.Prim.TyCon
import DDC.Source.Tetra.Prim.Base
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Compounds

import DDC.Data.Pretty
import Data.Char
import Data.List
import Control.DeepSeq


---------------------------------------------------------------------------------------------------
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


---------------------------------------------------------------------------------------------------
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
kindPrimTyConTetra tc
 = case tc of
        PrimTyConTetraTuple n   -> foldr (~>) KData (replicate n KData)
        PrimTyConTetraVector    -> KRegion ~> KData ~> KData
        PrimTyConTetraF         -> KData   ~> KData
        PrimTyConTetraC         -> KData   ~> KData
        PrimTyConTetraU         -> KData   ~> KData


---------------------------------------------------------------------------------------------------
pattern TVector   tR tA = TApp2 (TCon (TyConPrim (PrimTypeTyConTetra PrimTyConTetraVector))) tR tA
pattern TFunValue tA    = TApp  (TCon (TyConPrim (PrimTypeTyConTetra PrimTyConTetraF)))      tA
pattern TCloValue tA    = TApp  (TCon (TyConPrim (PrimTypeTyConTetra PrimTyConTetraC)))      tA

