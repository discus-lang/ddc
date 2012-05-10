
-- | Names shared by several language profiles.
module DDC.Core.Salt.Prim
        ( PrimTyCon     (..)
        , readPrimTyCon

        , PrimOp        (..)
        , readPrimOp

        , readLitInteger
        , readLitPrimWordOfBits
        , readLitPrimIntOfBits)
where
import DDC.Base.Pretty
import Data.Char
import Data.List


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | The Void type.
        = PrimTyConVoid

        -- | Type of store pointers
        | PrimTyConPtr

        -- | Type of machine addresses.
        | PrimTyConAddr

        -- | Type of natural numbers,
        --   Used for field indices and general counters.
        | PrimTyConNat

        -- | Type of data type tags.
        | PrimTyConTag

        -- | Type of booleans.
        | PrimTyConBool

        -- | String of UTF8 characters.
        | PrimTyConString 

        -- | Unsigned words of the given length.
        | PrimTyConWord   Int

        -- | Signed integers of the given length.
        | PrimTyConInt    Int

        -- | Floating point numbers of the given length.
        | PrimTyConFloat  Int

        deriving (Eq, Ord, Show)


instance Pretty PrimTyCon where
 ppr tc
  = case tc of
        PrimTyConVoid           -> text "Void#"
        PrimTyConPtr            -> text "Ptr#"
        PrimTyConAddr           -> text "Addr#"
        PrimTyConNat            -> text "Nat#"
        PrimTyConTag            -> text "Tag#"
        PrimTyConBool           -> text "Bool#"
        PrimTyConString         -> text "String#"
        PrimTyConWord   bits    -> text "Word"  <> int bits <> text "#"
        PrimTyConInt    bits    -> text "Int"   <> int bits <> text "#"
        PrimTyConFloat  bits    -> text "Float" <> int bits <> text "#"


-- | Read a primitive typ constructor.
readPrimTyCon :: String -> Maybe PrimTyCon
readPrimTyCon str
        | str == "Void#"   = Just $ PrimTyConVoid
        | str == "Ptr#"    = Just $ PrimTyConPtr
        | str == "Addr#"   = Just $ PrimTyConAddr
        | str == "Nat#"    = Just $ PrimTyConNat
        | str == "Tag#"    = Just $ PrimTyConTag
        | str == "Bool#"   = Just $ PrimTyConBool
        | str == "String#" = Just $ PrimTyConString

        -- WordN#
        | Just rest     <- stripPrefix "Word" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ PrimTyConWord n

        -- IntN#
        | Just rest     <- stripPrefix "Int" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [8, 16, 32, 64]
        = Just $ PrimTyConInt n

        -- FloatN#
        | Just rest     <- stripPrefix "Float" str
        , (ds, "#")     <- span isDigit rest
        , not $ null ds
        , n             <- read ds
        , elem n [32, 64]
        = Just $ PrimTyConInt n

        | otherwise
        = Nothing


-- PrimOp ---------------------------------------------------------------------
-- | Primitive numeric, comparison or logic operators.
--   We expect the backend/machine to be able to implement these directly.
data PrimOp
        -- arithmetic
        = PrimOpNeg
        | PrimOpAdd
        | PrimOpSub
        | PrimOpMul
        | PrimOpDiv
        | PrimOpRem

        -- comparison
        | PrimOpEq
        | PrimOpNeq
        | PrimOpGt
        | PrimOpGe
        | PrimOpLt
        | PrimOpLe

        -- boolean
        | PrimOpAnd
        | PrimOpOr

        -- bitwise
        | PrimOpShl
        | PrimOpShr
        | PrimOpBAnd
        | PrimOpBOr
        | PrimOpBXOr
        deriving (Eq, Ord, Show)


instance Pretty PrimOp where
 ppr op
  = let Just (_, n) = find (\(p, _) -> op == p) primOpNames
    in  (text n)


-- | Read a primitive operator.
readPrimOp :: String -> Maybe PrimOp
readPrimOp str
  =  case find (\(_, n) -> str == n) primOpNames of
        Just (p, _)     -> Just p
        _               -> Nothing


-- | Names of primitve operators.
primOpNames :: [(PrimOp, String)]
primOpNames
 =      [ (PrimOpNeg,           "neg#")
        , (PrimOpAdd,           "add#")
        , (PrimOpSub,           "sub#")
        , (PrimOpMul,           "mul#")
        , (PrimOpDiv,           "div#")
        , (PrimOpRem,           "rem#")
        , (PrimOpEq ,           "eq#" )
        , (PrimOpNeq,           "neq#")
        , (PrimOpGt ,           "gt#" )
        , (PrimOpLt ,           "lt#" )
        , (PrimOpLe ,           "le#" )
        , (PrimOpAnd,           "and#")
        , (PrimOpOr ,           "or#" ) 
        , (PrimOpShl,           "shl#")
        , (PrimOpShr,           "shr#")
        , (PrimOpBAnd,          "band#")
        , (PrimOpBOr,           "bor#")
        , (PrimOpBXOr,          "bxor#") ]


-- Literals -------------------------------------------------------------------
-- | Read a signed integer.
readLitInteger :: String -> Maybe Integer
readLitInteger []       = Nothing
readLitInteger str@(c:cs)
        | '-'   <- c
        , all isDigit cs
        = Just $ read str

        | all isDigit str
        = Just $ read str
        
        | otherwise
        = Nothing
        

-- | Read a word with an explicit format speficier.
readLitPrimWordOfBits :: String -> Maybe (Integer, Int)
readLitPrimWordOfBits str1
        -- binary like 0b01001w32#
        | Just str2     <- stripPrefix "0b" str1
        , (ds, str3)    <- span (\c -> c == '0' || c == '1') str2
        , not $ null ds
        , Just str4     <- stripPrefix "w" str3
        , (bs, "#")     <- span isDigit str4
        , not $ null bs
        , bits          <- read bs
        , length ds     <= bits
        = Just (readBinary ds, bits)

        -- decimal like 1234w32#
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just str3     <- stripPrefix "w" str2
        , (bs, "#")     <- span isDigit str3
        , not $ null bs
        = Just (read ds, read bs)

        | otherwise
        = Nothing

readBinary :: (Num a, Read a) => String -> a
readBinary digits
        = foldl' (\ acc b -> if b then 2 * acc + 1 else 2 * acc) 0
        $ map (/= '0') digits


-- | Read an integer with an explicit format specifier like @1234i32@.
--- TODO hande negative literals.
readLitPrimIntOfBits :: String -> Maybe (Integer, Int)
readLitPrimIntOfBits str1
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just str3     <- stripPrefix "i" str2
        , (bs, "#")     <- span isDigit str3
        , not $ null bs
        = Just $ (read ds, read bs)

        | otherwise
        = Nothing
