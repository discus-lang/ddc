
module DDC.Core.Tetra.Prim.TyConTetra
        ( kindTyConTetra
        , readTyConTetra
        , tRef
        , tTupleN
        , tBoxed
        , tUnboxed)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Compounds.Annot
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


instance NFData TyConTetra

instance Pretty TyConTetra where
 ppr tc
  = case tc of
        TyConTetraRef           -> text "Ref#"
        TyConTetraTuple n       -> text "Tuple" <> int n <> text "#"
        TyConTetraB             -> text "B#"
        TyConTetraU             -> text "U#"


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
                "B#"            -> Just TyConTetraB
                "U#"            -> Just TyConTetraU
                _               -> Nothing


-- | Take the kind of a baked-in type constructor.
kindTyConTetra :: TyConTetra -> Type Name
kindTyConTetra tc
 = case tc of
        TyConTetraRef     -> kRegion `kFun` kData `kFun` kData
        TyConTetraTuple n -> foldr kFun kData (replicate n kData)
        TyConTetraB       -> kData   `kFun` kData
        TyConTetraU       -> kData   `kFun` kData


-- Compounds ------------------------------------------------------------------
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA
 = tApps (TCon (TyConBound (UPrim (NameTyConTetra TyConTetraRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData


tTupleN :: [Type Name] -> Type Name
tTupleN tys     = tApps (tConTyConTetra (TyConTetraTuple (length tys))) tys


tBoxed  :: Type Name -> Type Name
tBoxed t        = tApp (tConTyConTetra TyConTetraB) t


tUnboxed :: Type Name -> Type Name
tUnboxed t      = tApp (tConTyConTetra TyConTetraU) t


-- Utils ----------------------------------------------------------------------
tConTyConTetra :: TyConTetra -> Type Name
tConTyConTetra tcf
 = let  k       = kindTyConTetra tcf
        u       = UPrim (NameTyConTetra tcf) k
        tc      = TyConBound u k
   in   TCon tc
