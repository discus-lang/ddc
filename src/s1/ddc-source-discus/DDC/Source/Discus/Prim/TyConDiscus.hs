{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive type constructors for Source Discus language.
module DDC.Source.Discus.Prim.TyConDiscus
        ( kindPrimTyConDiscus
        , readPrimTyConDiscus
        , pattern TVector
        , pattern TFunValue)
where
import DDC.Source.Discus.Prim.TyCon
import DDC.Source.Discus.Prim.Base
import DDC.Source.Discus.Exp.Generic
import DDC.Source.Discus.Exp.Compounds

import DDC.Data.Pretty
import Data.Char
import Data.List
import Control.DeepSeq


---------------------------------------------------------------------------------------------------
instance NFData PrimTyConDiscus where
 rnf !_ = ()


instance Pretty PrimTyConDiscus where
 ppr tc
  = case tc of
        PrimTyConDiscusTuple n   -> text "Tuple" <> int n
        PrimTyConDiscusVector    -> text "Vector"
        PrimTyConDiscusF         -> text "F#"
        PrimTyConDiscusU         -> text "U#"


---------------------------------------------------------------------------------------------------
-- | Read the name of a baked-in type constructor.
readPrimTyConDiscus :: String -> Maybe PrimTyConDiscus
readPrimTyConDiscus str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ PrimTyConDiscusTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just PrimTyConDiscusVector
                "F#"            -> Just PrimTyConDiscusF
                "U#"            -> Just PrimTyConDiscusU
                _               -> Nothing


-- | Take the kind of a baked-in data constructor.
kindPrimTyConDiscus tc
 = case tc of
        PrimTyConDiscusTuple n   -> foldr (~>) KData (replicate n KData)
        PrimTyConDiscusVector    -> KRegion ~> KData ~> KData
        PrimTyConDiscusF         -> KData   ~> KData
        PrimTyConDiscusU         -> KData   ~> KData


---------------------------------------------------------------------------------------------------
pattern TVector   tR tA = TApp2 (TCon (TyConPrim (PrimTypeTyConDiscus PrimTyConDiscusVector))) tR tA
pattern TFunValue tA    = TApp  (TCon (TyConPrim (PrimTypeTyConDiscus PrimTyConDiscusF)))      tA

