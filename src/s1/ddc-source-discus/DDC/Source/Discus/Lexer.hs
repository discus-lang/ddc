
-- | Lexer for Source Discus tokens.
module DDC.Source.Discus.Lexer
        ( Name (..)
        , lexModuleString

        , readName
        , readPrimType
        , readPrimLit
        , readPrimVal
        , readPrimTyConDiscus)
where
import DDC.Source.Discus.Exp.Type.Prim
import DDC.Source.Discus.Exp.Type.Pretty        ()
import DDC.Source.Discus.Exp.Type.NFData        ()

import DDC.Source.Discus.Exp.Term.Prim
import DDC.Source.Discus.Exp.Term.Pretty        ()
import DDC.Source.Discus.Exp.Term.NFData        ()

import DDC.Core.Codec.Text.Lexer
import DDC.Data.Pretty
import Control.DeepSeq
import Data.Char
import Data.Text                (Text)
import Data.List
import qualified Data.Text      as Text

import DDC.Core.Discus.Prim
        ( readPrimTyCon
        , readPrimCastFlag
        , readPrimArithFlag
        , readOpFun
        , readOpErrorFlag
        , readOpVectorFlag)

---------------------------------------------------------------------------------------------------
-- | Union of all names that we detect during lexing.
data Name
        -- | A user defined variable.
        = NameVar        !Text

        -- | A user defined constructor.
        | NameCon        !Text

        -- | Primitive type names.
        | NamePrimType   !TyConPrim

        -- | Primitive literal values.
        | NamePrimValLit !PrimLit

        -- | Primitive operator values.
        | NamePrimValOp  !PrimVal
        deriving (Eq, Ord, Show)


---------------------------------------------------------------------------------------------------
instance Pretty Name where
 ppr nn
  = case nn of
        NameVar  v              -> text (Text.unpack v)
        NameCon  c              -> text (Text.unpack c)
        NamePrimType p          -> ppr p
        NamePrimValLit p        -> ppr p
        NamePrimValOp  p        -> ppr p


instance NFData Name where
 rnf nn
  = case nn of
        NameVar s               -> rnf s
        NameCon s               -> rnf s
        NamePrimType p          -> rnf p
        NamePrimValLit _        -> ()
        NamePrimValOp  _        -> ()


-- | Read the name of a variable, constructor or literal.
readName :: String -> Maybe Name
readName str
        -- Primitive names.
        | Just n        <- readPrimType str
        = Just $ NamePrimType   n

        | Just n        <- readPrimLit str
        = Just $ NamePrimValLit n

        | Just n        <- readPrimVal str
        = Just $ NamePrimValOp  n

        -- Constructors.
        | c : _         <- str
        , isUpper c
        = Just $ NameCon (Text.pack str)

        -- Variables.
        | c : _         <- str
        , isVarStart c
        = Just $ NameVar (Text.pack str)

        | otherwise
        = Nothing


-- | Read the name of a primitive type.
readPrimType :: String -> Maybe TyConPrim
readPrimType str
        | Just p <- readPrimTyConDiscus str
        = Just $ TyConPrimDiscus p

        | Just p <- readPrimTyCon str
        = Just $ TyConPrimTyCon p

        | otherwise
        = Nothing


-- | Read the name of a primitive literal.
readPrimLit :: String -> Maybe PrimLit
readPrimLit str
        -- Literal Bools
        | str == "True"        = Just $ PrimLitBool True
        | str == "False"       = Just $ PrimLitBool False

        -- Literal Nat
        | Just val <- readLitNat str
        = Just $ PrimLitNat  val

        -- Literal Ints
        | Just val <- readLitInt str
        = Just $ PrimLitInt  val

        -- Literal Sizes
        | Just val <- readLitSize str
        = Just $ PrimLitSize val

        -- Literal Words
        | Just (val, bits) <- readLitWordOfBits str
        , elem bits [8, 16, 32, 64]
        = Just $ PrimLitWord val bits

        -- Literal Floats
        | Just (val, mbits) <- readLitFloatOfBits str
        = case mbits of
                Just 32         -> Just $ PrimLitFloat val 32
                Just 64         -> Just $ PrimLitFloat val 64
                Nothing         -> Just $ PrimLitFloat val 64
                _               -> Nothing

        | otherwise
        = Nothing


-- | Read the name of a primtive value.
readPrimVal :: String -> Maybe PrimVal
readPrimVal str
        | Just (p, False) <- readOpErrorFlag str
        = Just $ PrimValError  p

        | Just lit        <- readPrimLit str
        = Just $ PrimValLit    lit

        | Just (p, False) <- readPrimArithFlag str
        = Just $ PrimValArith  p

        | Just (p, False) <- readPrimCastFlag  str
        = Just $ PrimValCast   p

        | Just (p, False) <- readOpVectorFlag  str
        = Just $ PrimValVector p

        | Just p          <- readOpFun str
        = Just $ PrimValFun    p

        | otherwise
        = Nothing


-- | Read the name of a baked-in type constructor.
readPrimTyConDiscus :: String -> Maybe TyConDiscus
readPrimTyConDiscus str
        | Just rest     <- stripPrefix "Tuple" str
        , (ds, "")      <- span isDigit rest
        , not $ null ds
        , arity         <- read ds
        = Just $ TyConDiscusTuple arity

        | otherwise
        = case str of
                "Vector#"       -> Just TyConDiscusVector
                "F#"            -> Just TyConDiscusF
                "U#"            -> Just TyConDiscusU
                _               -> Nothing


---------------------------------------------------------------------------------------------------
-- | Lex a string to tokens, using primitive names.
--
--   The first argument gives the starting source line number.
--
--   We're currently re-using the lexer for the core language, which has
--   *mostly* the same lexical structure as Source Discus.
--   There are a few tokens accepted by one language but not the other,
--   but it'll do for now.
--
lexModuleString :: String -> Int -> String -> [Located (Token Name)]
lexModuleString sourceName lineStart str
 = map rn $ lexModuleWithOffside sourceName lineStart str
 where
        rn (Located sp strTok)
         = case renameToken readName strTok of
                Just t' -> Located sp t'
                Nothing -> Located sp (KErrorJunk "lexical error")


