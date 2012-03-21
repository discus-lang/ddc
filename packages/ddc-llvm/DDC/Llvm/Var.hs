
module DDC.Llvm.Var
        ( module DDC.Llvm.Attr
        , module DDC.Llvm.Type

          -- * Uniques
        , Unique

          -- * Sections
        , Section       (..)

          -- * Variables
        , Var           (..)
        , Ptr, Value

        , isGlobal
        , linkageOfVar
        , nameOfVar
        , typeOfVar

          -- * Literals
        , Lit           (..)
        , typeOfLit)
where
import DDC.Llvm.Attr
import DDC.Llvm.Type
import DDC.Base.Pretty
import Data.Char
import Numeric
import Data.Array.ST
import Control.Monad.ST
import Data.Word
import qualified Data.Array.Unsafe      as Unsafe


-- Unique ---------------------------------------------------------------------
-- | Unique id.
type Unique      = Int


-- Section --------------------------------------------------------------------
-- | The section name to put the function in.
data Section
        -- | Let the LLVM decide what section to put this in.
        = SectionAuto

        -- | Put it in this specific section.
        | SectionSpecific String
        deriving (Eq, Show)


-- Var ------------------------------------------------------------------------
-- | Llvm Variables
data Var
        -- | Variables with a global scope.
        = VarGlobal
                String 
                Type 
                Linkage
                Section
                Align
                LMConst

        -- | Variables local to a function or parameters.
        | VarLocal      String  Type

        -- | A constant variable
        | VarLit        Lit
        deriving (Eq, Show)

type Ptr        = Var
type Value      = Var

instance Pretty Var where
 ppr lv
   = case lv of
        VarLit x        -> ppr x
        _               -> ppr (typeOfVar lv) <+> text (nameOfVar lv)


-- | Determines whether a variable is a constant or not.
type LMConst    = Bool


-- | Return the variable name or value of the 'LlvmVar'
--   in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
nameOfVar :: Var -> String
nameOfVar v
 = case v of
        VarGlobal{}     -> "@" ++ plainNameOfVar v
        VarLocal{}      -> "%" ++ plainNameOfVar v
        VarLit{}        -> plainNameOfVar v


-- | Return the variable name or value of the 'LlvmVar'
--   in a plain textual representation (e.g. @x@, @y@ or @42@).
plainNameOfVar :: Var -> String
plainNameOfVar vv
 = case vv of
        VarGlobal x _ _ _ _ _   -> x
        VarLocal  x _           -> x
        VarLit    x             -> showLit x


-- | Return the 'LlvmType' of the 'LlvmVar'
typeOfVar :: Var -> Type
typeOfVar vv
 = case vv of
        VarGlobal _ t _ _ _ _   -> t
        VarLocal  _ t           -> t
        VarLit l                -> typeOfLit l


-- | Test if a 'LlvmVar' is global.
isGlobal :: Var -> Bool
isGlobal vv
 = case vv of
        VarGlobal{}             -> True
        _                       -> False

-- | Yield the linkage of a var.
linkageOfVar :: Var -> Linkage
linkageOfVar vv
 = case vv of
        VarGlobal _ _ l _ _ _   -> l
        _                       -> Internal


-- Lit --------------------------------------------------------------------------------------------
-- | Llvm Literal Data.
--   These can be used inline in expressions.
data Lit
        -- | Refers to an integer constant (i64 42).
        = LMIntLit   Integer Type

        -- | Floating point literal
        | LMFloatLit Double  Type

        -- | Literal NULL, only applicable to pointer types
        | LMNullLit  Type

        -- | Undefined value, random bit pattern. Useful for optimisations.
        | LMUndefLit Type
        deriving (Eq, Show)


instance Pretty Lit where
  ppr l = ppr (typeOfLit l) <+> text (showLit l)


-- | Return the 'LlvmType' of a 'LlvmLit'
typeOfLit :: Lit -> Type
typeOfLit ll
 = case ll of
        LMIntLit   _ t          -> t
        LMFloatLit _ t          -> t
        LMNullLit    t          -> t
        LMUndefLit   t          -> t


-- | Print a literal value. No type.
showLit :: Lit -> String
showLit ll
 = case ll of
        LMIntLit   i _          -> show i
        LMFloatLit r TFloat     -> fToStr $ realToFrac r
        LMFloatLit r TDouble    -> dToStr r
        LMFloatLit _ _          -> error $ "Can't print this float literal!" ++ show ll
        LMNullLit _             -> "null"
        LMUndefLit _            -> "undef"


-- | Convert a Haskell Double to an LLVM hex encoded floating point form. In    -- TODO: shift this into pretty module
--   Llvm float literals can be printed in a big-endian hexadecimal format,
--   regardless of underlying architecture.
{-# NOINLINE dToStr #-}
dToStr :: Double -> String
dToStr d
  = let bs     = doubleToBytes d
        hex d' = case showHex d' "" of
                     []    -> error "dToStr: too few hex digits for float"
                     [x]   -> ['0',x]
                     [x,y] -> [x,y]
                     _     -> error "dToStr: too many hex digits for float"

        str  = map toUpper $ concat . fixEndian . (map hex) $ bs
    in  "0x" ++ str


-- | Convert a Haskell Float to an LLVM hex encoded floating point form.
--   LLVM uses the same encoding for both floats and doubles (16 digit hex
--   string) but floats must have the last half all zeroes so it can fit into
--   a float size type.
{-# NOINLINE fToStr #-}
fToStr :: Float -> String
fToStr = (dToStr . realToFrac)


-- | Reverse or leave byte data alone to fix endianness on this target.         -- TODO: eliminate this
fixEndian :: [a] -> [a]
-- #ifdef WORDS_BIGENDIAN
-- fixEndian = id
-- #else
fixEndian = reverse
-- #endif


doubleToBytes :: Double -> [Int]                                                -- TODO: shift this somewhere else
doubleToBytes d
   = runST (do
        arr' <- newArray_ ((0::Int),7)
        writeArray arr' 0 d
        arr <- castDoubleToWord8Array arr'
        i0  <- readArray arr 0
        i1' <- readArray arr 1
        i2  <- readArray arr 2
        i3  <- readArray arr 3
        i4  <- readArray arr 4
        i5  <- readArray arr 5
        i6  <- readArray arr 6
        i7  <- readArray arr 7
        return (map fromIntegral [i0,i1',i2,i3,i4,i5,i6,i7])
     )

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = Unsafe.castSTUArray

