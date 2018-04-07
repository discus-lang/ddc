
-- | Lexer for Source Discus tokens.
module DDC.Source.Discus.Lexer
        ( Name (..)
        , lexModuleString)
where
import DDC.Source.Discus.Prim
import DDC.Source.Discus.Exp.Type.Pretty        ()
import DDC.Source.Discus.Exp.Type.NFData        ()
import DDC.Core.Codec.Text.Lexer
import DDC.Data.Pretty
import Control.DeepSeq
import Data.Char
import Data.Text                (Text)
import qualified Data.Text      as Text


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
        NamePrimValLit p        -> rnf p
        NamePrimValOp  p        -> rnf p


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


