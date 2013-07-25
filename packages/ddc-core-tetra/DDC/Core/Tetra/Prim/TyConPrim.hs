
module DDC.Core.Tetra.Prim.TyConPrim 
        ( TyConPrim     (..)
        , readTyConPrim
        , kindTyConPrim
        , tBool
        , tNat
        , tInt
        , tWord
        , tRef)
where
import DDC.Core.Tetra.Prim.Base
import DDC.Core.Compounds.Annot
import DDC.Core.Exp.Simple
import DDC.Base.Pretty
import Control.DeepSeq
import Data.List
import Data.Char


instance NFData TyConPrim where
 rnf tc
  = case tc of
        TyConPrimWord i         -> rnf i
        _                       -> ()


instance Pretty TyConPrim where
 ppr tc
  = case tc of
        TyConPrimBool           -> text "Bool"
        TyConPrimNat            -> text "Nat"
        TyConPrimInt            -> text "Int"
        TyConPrimWord   bits    -> text "Word"  <> int bits
        TyConPrimRef            -> text "Ref"


-- | Read a primitive type constructor.
--  
--   Words are limited to 8, 16, 32, or 64 bits.
--  
--   Floats are limited to 32 or 64 bits.
readTyConPrim :: String -> Maybe TyConPrim
readTyConPrim str
        | str == "Bool" = Just $ TyConPrimBool
        | str == "Nat"  = Just $ TyConPrimNat
        | str == "Int"  = Just $ TyConPrimInt

        -- WordN
        | Just rest     <- stripPrefix "Word" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ TyConPrimWord n

        | str == "Ref"  = Just $ TyConPrimRef

        | otherwise
        = Nothing


-- | Yield the kind of a type constructor.
kindTyConPrim :: TyConPrim -> Kind Name
kindTyConPrim tc
 = case tc of
        TyConPrimBool    -> kData
        TyConPrimNat     -> kData
        TyConPrimInt     -> kData
        TyConPrimWord  _ -> kData
        TyConPrimRef     -> kRegion `kFun` kData `kFun` kData


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimBool) kData) kData)


-- | Primitive `Nat` type.
tNat    ::  Type Name
tNat    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimNat) kData) kData)


-- | Primitive `Int` type.
tInt    ::  Type Name
tInt    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimInt) kData) kData)


-- | Primitive `WordN` type of the given width.
tWord   :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NameTyConPrim (TyConPrimWord bits)) kData) kData)


-- | Primitive `Ref` type.
tRef    :: Region Name -> Type Name -> Type Name
tRef tR tA   
 = tApps (TCon (TyConBound (UPrim (NameTyConPrim TyConPrimRef) k) k))
                [tR, tA]
 where k = kRegion `kFun` kData `kFun` kData

