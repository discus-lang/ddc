
-- | Names shared by several language profiles.
module DDC.Core.Salt.Prim
        ( PrimTyCon     (..)
        , readPrimTyCon

        , PrimOp        (..)
        , readPrimOp

        , readLitInteger
        , readLitPrimNat
        , readLitPrimInt
        , readLitPrimWordOfBits)
where
import DDC.Base.Pretty
import Data.Char
import Data.List


-- PrimTyCon -----------------------------------------------------------------
-- | Primitive type constructors.
data PrimTyCon
        -- | The Void type.
        = PrimTyConVoid

        -- | Type of booleans.
        | PrimTyConBool

        -- | Type of natural numbers,
        --   Big enough to count every addressable byte in the system.
        | PrimTyConNat

        -- | Type of signed integers,
        | PrimTyConInt

        -- | Machine words of the given length.
        | PrimTyConWord   Int

        -- | Floating point numbers of the given length.
        | PrimTyConFloat  Int

        -- | Type of data type tags.
        | PrimTyConTag

        -- | Type of machine addresses.
        | PrimTyConAddr

        -- | Type of store pointers
        | PrimTyConPtr

        -- | String of UTF8 characters.
        --   TODO: We'll stop treating this as primitive once we can define 
        --         our own types.
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


-- | Read a primitive typ constructor.
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
        , (PrimOpGe ,           "ge#" )
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
        

-- | Read an integer with an explicit format specifier like @1234i#@.
--- TODO hande negative literals.
readLitPrimNat :: String -> Maybe Integer
readLitPrimNat str1
        | (ds, str2)    <- span isDigit str1
        , not $ null ds
        , Just ""       <- stripPrefix "#" str2
        = Just $ read ds

        | otherwise
        = Nothing

-- | Read an integer with an explicit format specifier like @1234i#@.
readLitPrimInt :: String -> Maybe Integer
readLitPrimInt str1
        | '-' : str2    <- str1
        , (ds, "i#")    <- span isDigit str2
        , not $ null ds
        = Just $ read ds

        | (ds, "i#")    <- span isDigit str1
        , not $ null ds
        = Just $ read ds

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
