
module DDC.Core.Salt.Name.PrimTyCon
        ( PrimTyCon     (..)
        , pprPrimTyConStem
        , readPrimTyCon
        , readPrimTyConStem
        , primTyConIsIntegral
        , primTyConIsFloating
        , primTyConIsUnsigned
        , primTyConIsSigned
        , primTyConWidth)
where
import DDC.Core.Salt.Platform
import DDC.Data.ListUtils
import DDC.Base.Pretty
import Control.DeepSeq
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
        --   Enough precision to count every object in the heap,
        --   but NOT necessearily enough precision to count every byte of memory.
        | PrimTyConNat

        -- | @Int#@ signed integers.
        --   Enough precision to count every object in the heap,
        --   but NOT necessearily enough precision to count every byte of memory.
        --   If N is the total number of objects that can exist in the heap,
        --   then the range of @Int#@ is at least (-N .. +N) inclusive.
        | PrimTyConInt

        -- | @Size#@ unsigned sizes.
        --   Enough precision to count every addressable bytes of memory.
        | PrimTyConSize

        -- | @WordN#@ machine words of the given width.
        | PrimTyConWord   Int

        -- | @FloatN#@ floating point numbers of the given width.
        | PrimTyConFloat  Int

        -- | @VecN#@ a packed vector of N values.
        --   This is intended to have kind (Data -> Data), 
        --   so we use concrete vector types like @Vec4# Word32#@.
        | PrimTyConVec    Int

        -- | @Addr#@ a relative or absolute machine address.
        --   Enough precision to count every byte of memory.
        --   Unlike pointers below, an absolute @Addr#@ need not refer to 
        --   memory owned by the current process.
        | PrimTyConAddr

        -- | @Ptr#@ like @Addr#@, but with a region and element type annotation.
        --   In particular, a value of a type like (Ptr# r Word32#) must be at least
        --   4-byte aligned and point to memory owned by the current process.
        | PrimTyConPtr

        -- | @TextLit#@ type of a text literal, which is represented as a pointer
        --   to the literal data in static memory.
        | PrimTyConTextLit

        -- | @Tag#@ data constructor tags.
        --   Enough precision to count every possible alternative of an 
        --   enumerated type.
        | PrimTyConTag
        deriving (Eq, Ord, Show)


instance NFData PrimTyCon where
 rnf tc
  = case tc of
        PrimTyConWord i         -> rnf i
        PrimTyConFloat i        -> rnf i
        _                       -> ()


instance Pretty PrimTyCon where
 ppr tc = pprPrimTyConStem tc <> text "#"


-- | Pretty print a primitive type constructor, 
--   without the '#' suffix.
pprPrimTyConStem :: PrimTyCon -> Doc
pprPrimTyConStem tc
 = case tc of
        PrimTyConVoid           -> text "Void"
        PrimTyConBool           -> text "Bool"
        PrimTyConNat            -> text "Nat"
        PrimTyConInt            -> text "Int"
        PrimTyConSize           -> text "Size"
        PrimTyConWord   bits    -> text "Word"  <> int bits
        PrimTyConFloat  bits    -> text "Float" <> int bits
        PrimTyConVec    arity   -> text "Vec"   <> int arity
        PrimTyConTag            -> text "Tag"
        PrimTyConAddr           -> text "Addr"
        PrimTyConTextLit        -> text "TextLit"
        PrimTyConPtr            -> text "Ptr"


-- | Read a primitive type constructor.
--  
--   Words are limited to 8, 16, 32, or 64 bits.
--  
--   Floats are limited to 32 or 64 bits.
readPrimTyCon :: String -> Maybe PrimTyCon
readPrimTyCon str
        | Just stem  <- stripSuffix "#" str
        = readPrimTyConStem stem

        | otherwise
        = Nothing


-- | Read a primitive type constructor, without the '#' suffix.
readPrimTyConStem :: String -> Maybe PrimTyCon
readPrimTyConStem str
        | str == "Void"         = Just $ PrimTyConVoid
        | str == "Bool"         = Just $ PrimTyConBool
        | str == "Nat"          = Just $ PrimTyConNat
        | str == "Int"          = Just $ PrimTyConInt
        | str == "Size"         = Just $ PrimTyConSize
        | str == "Tag"          = Just $ PrimTyConTag
        | str == "Addr"         = Just $ PrimTyConAddr
        | str == "Ptr"          = Just $ PrimTyConPtr
        | str == "TextLit"      = Just $ PrimTyConTextLit

        -- WordN#
        | Just rest     <- stripPrefix "Word" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ PrimTyConWord n

        -- FloatN#
        | Just rest     <- stripPrefix "Float" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [32, 64]
        = Just $ PrimTyConFloat n

        -- VecN#
        | Just rest     <- stripPrefix "Vec" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [2, 4, 8, 16]        
        = Just $ PrimTyConVec n

        | otherwise
        = Nothing


-- | Integral constructors are the ones that we can reasonably
--   convert from integers of the same size. 
--  
--   These are @Bool#@, @Nat#@, @Int#@, @Size@, @WordN#@ and @Tag#@.
--
primTyConIsIntegral :: PrimTyCon -> Bool
primTyConIsIntegral tc
 = case tc of
        PrimTyConBool           -> True
        PrimTyConNat            -> True
        PrimTyConInt            -> True
        PrimTyConSize           -> True
        PrimTyConWord{}         -> True
        PrimTyConTag            -> True
        _                       -> False


-- | Floating point types.
-- 
--   These are @FloatN#@.
primTyConIsFloating :: PrimTyCon -> Bool
primTyConIsFloating tc
 = case tc of
        PrimTyConFloat{}        -> True
        _                       -> False


-- | Unsigned types.
--
--   These are @Bool#@ @Nat#@ @Size#@ @WordN@ @Tag@.
primTyConIsUnsigned :: PrimTyCon -> Bool
primTyConIsUnsigned tc
 = case tc of
        PrimTyConBool           -> True
        PrimTyConNat            -> True
        PrimTyConSize           -> True
        PrimTyConWord{}         -> True
        PrimTyConTag            -> True
        _                       -> False

-- | Signed integral constructors.
-- 
--   This is just @Int@.
primTyConIsSigned :: PrimTyCon -> Bool
primTyConIsSigned tc
 = case tc of
        PrimTyConInt            -> True
        PrimTyConFloat{}        -> True
        _                       -> False


-- | Get the representation width of a primitive type constructor, 
--   in bits. This is how much space it takes up in an object payload.
--
--   Bools are representable with a single bit, but we unpack
--   them into a whole word.
--
--   The constructors @Void@ and @VecN#@ and @String@ have no width.
--
primTyConWidth :: Platform -> PrimTyCon -> Maybe Integer
primTyConWidth pp tc
 = case tc of
        PrimTyConVoid           -> Nothing
        PrimTyConBool           -> Just $ 8 * platformNatBytes  pp 
        PrimTyConNat            -> Just $ 8 * platformNatBytes  pp
        PrimTyConInt            -> Just $ 8 * platformNatBytes  pp
        PrimTyConSize           -> Just $ 8 * platformNatBytes  pp
        PrimTyConWord  bits     -> Just $ fromIntegral bits
        PrimTyConFloat bits     -> Just $ fromIntegral bits
        PrimTyConTag            -> Just $ 8 * platformTagBytes  pp
        PrimTyConAddr           -> Just $ 8 * platformAddrBytes pp
        PrimTyConPtr            -> Just $ 8 * platformAddrBytes pp

        -- The string literal itself does not have a width associated with it.
        --  In the object code string literals are represented by pointers to
        --  static data. The static data is an array of Word8s, but the pointer
        --  itself is the width of an address on our machine.
        PrimTyConTextLit        -> Nothing


        PrimTyConVec   _        -> Nothing

