
module DDC.Source.Tetra.Prim.TyConTetra
        ( kindTyConTetra
        , readTyConTetra
        , tRef)
where
import DDC.Source.Tetra.Prim.Base
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Data.Char
import Data.List
import Control.DeepSeq

instance NFData TyConTetra

instance Pretty TyConTetra where
 ppr tc
  = case tc of
        TyConTetraRef           -> text "Ref#"
        TyConTetraTuple n       -> text "Tuple" <> int n <> text "#"


-- | Read the name of a baked-in type constructor.
readTyConTetra :: String -> Maybe TyConTetra
readTyConTetra str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConTetraTuple arity

        | otherwise
        = case str of
                "Ref#"          -> Just TyConTetraRef
                _               -> Nothing


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

