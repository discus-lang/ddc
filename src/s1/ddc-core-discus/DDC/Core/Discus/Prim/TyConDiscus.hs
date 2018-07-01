
module DDC.Core.Discus.Prim.TyConDiscus
        ( kindTyConDiscus
        , readTyConDiscus
        , tTupleN
        , tVector
        , tUnboxed
        , tFunValue)
where
import DDC.Core.Discus.Prim.Base
import DDC.Type.Exp.Simple
import DDC.Data.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


instance NFData TyConDiscus where
 rnf !_ = ()


instance Pretty TyConDiscus where
 ppr tc
  = case tc of
        TyConDiscusTuple n       -> text "Tuple" <> int n <> text "#"
        TyConDiscusVector        -> text "Vector#"
        TyConDiscusU             -> text "U#"
        TyConDiscusF             -> text "F#"


-- | Read the name of a baked-in type constructor.
readTyConDiscus :: String -> Maybe TyConDiscus
readTyConDiscus str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConDiscusTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just TyConDiscusVector
                "U#"            -> Just TyConDiscusU
                "F#"            -> Just TyConDiscusF
                _               -> Nothing


-- | Take the kind of a baked-in type constructor.
kindTyConDiscus :: TyConDiscus -> Type Name
kindTyConDiscus tc
 = case tc of
        TyConDiscusTuple n -> foldr kFun kData (replicate n kData)
        TyConDiscusVector  -> kRegion `kFun` kData `kFun` kData
        TyConDiscusU       -> kData   `kFun` kData
        TyConDiscusF       -> kData   `kFun` kData


-- Compounds ------------------------------------------------------------------
-- | Construct a tuple type.
tTupleN :: [Type Name] -> Type Name
tTupleN tys     = tApps (tConTyConDiscus (TyConDiscusTuple (length tys))) tys


-- | Construct a vector type.
tVector ::  Region Name -> Type Name -> Type Name
tVector tR tA   = tApps (tConTyConDiscus TyConDiscusVector) [tR, tA]


-- | Construct an unboxed representation type.
tUnboxed :: Type Name -> Type Name
tUnboxed t      = tApp (tConTyConDiscus TyConDiscusU) t


-- | Construct a reified function type.
tFunValue :: Type Name -> Type Name
tFunValue t     = tApp (tConTyConDiscus TyConDiscusF) t


-- Utils ----------------------------------------------------------------------
tConTyConDiscus :: TyConDiscus -> Type Name
tConTyConDiscus tcf
 = let  k       = kindTyConDiscus tcf
        u       = UName (NameTyConDiscus tcf)
        tc      = TyConBound u k
   in   TCon tc
