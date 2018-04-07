{-# LANGUAGE TypeFamilies #-}
-- | Definitions of primitive type constructors for Source Discus language.
module DDC.Source.Discus.Exp.Type.Prim.TyConDiscus
        ( kindPrimTyConDiscus
        , readPrimTyConDiscus
        , pattern TVector
        , pattern TFunValue)
where
import DDC.Source.Discus.Exp.Type.Base
import DDC.Source.Discus.Exp.Term.Compounds

import Data.Char
import Data.List


-- | Read the name of a baked-in type constructor.
readPrimTyConDiscus :: String -> Maybe TyConDiscus
readPrimTyConDiscus str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConDiscusTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just TyConDiscusVector
                "F#"            -> Just TyConDiscusF
                "U#"            -> Just TyConDiscusU
                _               -> Nothing


-- | Take the kind of a baked-in data constructor.
kindPrimTyConDiscus tc
 = case tc of
        TyConDiscusTuple n      -> foldr (~>) KData (replicate n KData)
        TyConDiscusVector       -> KRegion ~> KData ~> KData
        TyConDiscusF            -> KData   ~> KData
        TyConDiscusU            -> KData   ~> KData

