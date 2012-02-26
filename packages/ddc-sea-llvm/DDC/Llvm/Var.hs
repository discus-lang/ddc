
module DDC.Llvm.Var
        ( -- * Uniques
          Unique

          -- * Variables
        , LlvmVar  (..)
        , LMSection
        , isGlobal
        , getLink
        , pVarLift
        , pVarLower
        , getVarType
        , getLitType

          -- * Literals
        , LlvmLit  (..))
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


-- Unique -----------------------------------------------------------------------------------------
-- | Unique id.
type Unique
        = Int


-- Var --------------------------------------------------------------------------------------------
-- | Llvm Variables
data LlvmVar
        -- | Variables with a global scope.
        = LMGlobalVar 
                LMString 
                LlvmType 
                LlvmLinkageType
                LMSection
                LMAlign
                LMConst

        -- | Variables local to a function or parameters.
        | LMLocalVar  Unique LlvmType

        -- | Named local variables. Sometimes we need to be able to explicitly name
        --   variables (e.g for function arguments).
        | LMNLocalVar LMString LlvmType

        -- | A constant variable
        | LMLitVar LlvmLit
        deriving (Eq, Show)


instance Pretty LlvmVar where
 ppr lv
   = case lv of
        LMLitVar x      -> ppr x
        _               -> ppr (getVarType lv) <+> text (getName lv)


-- | An LLVM section definition.
--   If Nothing then let LLVM decide the section
type LMSection  = Maybe LMString

-- | Determines whether a variable is a constant or not.
type LMConst    = Bool

-- | Return the variable name or value of the 'LlvmVar'
--   in Llvm IR textual representation (e.g. @\@x@, @%y@ or @42@).
getName :: LlvmVar -> String
getName v@(LMGlobalVar _ _ _ _ _ _)     = "@" ++ getPlainName v
getName v@(LMLocalVar  _ _        )     = "%" ++ getPlainName v
getName v@(LMNLocalVar _ _        )     = "%" ++ getPlainName v
getName v@(LMLitVar    _          )     = getPlainName v


-- | Return the variable name or value of the 'LlvmVar'
--   in a plain textual representation (e.g. @x@, @y@ or @42@).
getPlainName :: LlvmVar -> String
getPlainName (LMGlobalVar x _ _ _ _ _)  = x
getPlainName (LMLocalVar  x _        )  = show x
getPlainName (LMNLocalVar x _        )  = x
getPlainName (LMLitVar    x          )  = getLit x

-- | Return the 'LlvmType' of the 'LlvmVar'
getVarType :: LlvmVar -> LlvmType
getVarType (LMGlobalVar _ y _ _ _ _)    = y
getVarType (LMLocalVar  _ y        )    = y
getVarType (LMNLocalVar _ y        )    = y
getVarType (LMLitVar    l          )    = getLitType l

-- | Lower a variable of 'LMPointer' type.
pVarLower :: LlvmVar -> LlvmVar
pVarLower (LMGlobalVar s t l x a c) = LMGlobalVar s (pLower t) l x a c
pVarLower (LMLocalVar  s t        ) = LMLocalVar  s (pLower t)
pVarLower (LMNLocalVar s t        ) = LMNLocalVar s (pLower t)
pVarLower (LMLitVar    _          ) = error $ "Can't lower a literal type!"


-- | Test if a 'LlvmVar' is global.
isGlobal :: LlvmVar -> Bool
isGlobal (LMGlobalVar _ _ _ _ _ _) = True
isGlobal _                         = False

-- | Return the 'LlvmLinkageType' for a 'LlvmVar'
getLink :: LlvmVar -> LlvmLinkageType
getLink (LMGlobalVar _ _ l _ _ _) = l
getLink _                         = Internal

-- | Lower a variable of 'LMPointer' type.
pVarLift :: LlvmVar -> LlvmVar
pVarLift (LMGlobalVar s t l x a c) = LMGlobalVar s (pLift t) l x a c
pVarLift (LMLocalVar  s t        ) = LMLocalVar  s (pLift t)
pVarLift (LMNLocalVar s t        ) = LMNLocalVar s (pLift t)
pVarLift (LMLitVar    _          ) = error $ "Can't lower a literal type!"


-- Lit --------------------------------------------------------------------------------------------
-- | Llvm Literal Data.
--   These can be used inline in expressions.
data LlvmLit
        -- | Refers to an integer constant (i64 42).
        = LMIntLit   Integer LlvmType

        -- | Floating point literal
        | LMFloatLit Double  LlvmType

        -- | Literal NULL, only applicable to pointer types
        | LMNullLit  LlvmType

        -- | Undefined value, random bit pattern. Useful for optimisations.
        | LMUndefLit LlvmType
        deriving (Eq, Show)


instance Pretty LlvmLit where
  ppr l = ppr (getLitType l) <+> text (getLit l)


-- | Return the 'LlvmType' of a 'LlvmLit'
getLitType :: LlvmLit -> LlvmType
getLitType (LMIntLit   _ t)     = t
getLitType (LMFloatLit _ t)     = t
getLitType (LMNullLit    t)     = t
getLitType (LMUndefLit   t)     = t


-- | Print a literal value. No type.
getLit :: LlvmLit -> String
getLit (LMIntLit   i _       )          = show i
getLit (LMFloatLit r LMFloat )          = fToStr $ realToFrac r
getLit (LMFloatLit r LMDouble)          = dToStr r
getLit f@(LMFloatLit _ _)               = error $ "Can't print this float literal!" ++ show f
getLit (LMNullLit _     )               = "null"
getLit (LMUndefLit _    )               = "undef"


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

