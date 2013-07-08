
module DDC.Core.Blue.Prim.TyConPrim 
        ( TyConPrim     (..)
        , readTyConPrim
        , kindTyConPrim
        , tBool
        , tNat
        , tInt
        , tWord)
where
import DDC.Core.Blue.Prim.Base
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


-- | Read a primitive type constructor.
--  
--   Words are limited to 8, 16, 32, or 64 bits.
--  
--   Floats are limited to 32 or 64 bits.
readTyConPrim :: String -> Maybe TyConPrim
readTyConPrim str
        | str == "Bool"   = Just $ TyConPrimBool
        | str == "Nat"    = Just $ TyConPrimNat
        | str == "Int"    = Just $ TyConPrimInt

        -- WordN
        | Just rest     <- stripPrefix "Word" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ TyConPrimWord n

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


-- Compounds ------------------------------------------------------------------
-- | Primitive `Bool` type.
tBool   :: Type Name
tBool   = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimBool) kData) kData)


-- | Primitive `Nat` type.
tNat    ::  Type Name
tNat    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimInt) kData) kData)


-- | Primitive `Int` type.
tInt    ::  Type Name
tInt    = TCon (TyConBound (UPrim (NameTyConPrim TyConPrimInt) kData) kData)


-- | Primitive `WordN` type of the given width.
tWord :: Int -> Type Name
tWord bits 
        = TCon (TyConBound (UPrim (NameTyConPrim (TyConPrimWord bits)) kData) kData)

