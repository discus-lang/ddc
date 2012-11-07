
module DDC.Core.Salt.Name.PrimTyCon
        ( PrimTyCon     (..)
        , readPrimTyCon)
where
import DDC.Base.Pretty
import Data.Char
import Data.List


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | @Void#@ the Void type has no values.
        = PrimTyConVoid

        -- | @Bool#@ unboxed booleans.
        | PrimTyConBool

        -- | @Nat#@ natural numbers.
        --   Big enough to count every addressable byte in the system.
        | PrimTyConNat

        -- | @Int#@ signed integers.
        | PrimTyConInt

        -- | @WordN#@ machine words of the given length.
        | PrimTyConWord   Int

        -- | @FloatN#@ floating point numbers of the given length.
        | PrimTyConFloat  Int

        -- | @Tag#@ data type tags.
        | PrimTyConTag

        -- | @Addr#@ machine addresses.
        | PrimTyConAddr

        -- | @Ptr#@ store pointers.
        | PrimTyConPtr

        -- | @String#@ String of UTF8 characters.
        -- 
        --   These are primitive until we can define our own unboxed types.
        | PrimTyConString 
        deriving (Eq, Ord, Show)


instance Pretty PrimTyCon where
 ppr tc
  = case tc of
        PrimTyConVoid           -> text "Void#"
        PrimTyConBool           -> text "Bool#"
        PrimTyConNat            -> text "Nat#"
        PrimTyConInt            -> text "Int#"
        PrimTyConWord   bits    -> text "Word"  <> int bits <> text "#"
        PrimTyConFloat  bits    -> text "Float" <> int bits <> text "#"
        PrimTyConTag            -> text "Tag#"
        PrimTyConAddr           -> text "Addr#"
        PrimTyConPtr            -> text "Ptr#"
        PrimTyConString         -> text "String#"


-- | Read a primitive type constructor.
--  
--   Words are limited to 8, 16, 32, or 64 bits.
--  
--   Floats are limited to 32 or 64 bits.
readPrimTyCon :: String -> Maybe PrimTyCon
readPrimTyCon str
        | str == "Void#"   = Just $ PrimTyConVoid
        | str == "Bool#"   = Just $ PrimTyConBool
        | str == "Nat#"    = Just $ PrimTyConNat
        | str == "Int#"    = Just $ PrimTyConInt
        | str == "Tag#"    = Just $ PrimTyConTag
        | str == "Addr#"   = Just $ PrimTyConAddr
        | str == "Ptr#"    = Just $ PrimTyConPtr
        | str == "String#" = Just $ PrimTyConString

        -- WordN#
        | Just rest     <- stripPrefix "Word" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ PrimTyConWord n

        -- FloatN#
        | Just rest     <- stripPrefix "Float" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [32, 64]
        = Just $ PrimTyConFloat n

        | otherwise
        = Nothing

