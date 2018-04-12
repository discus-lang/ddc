
module DDC.Core.Codec.Text.Lexer.Tokens
        ( Located (..)
        , columnOfLocated

          -- * Tokens
        , Token         (..)
        , TokenMeta     (..)
        , TokenAtom     (..)
        , TokenNamed    (..)

        , Keyword       (..)
        , Symbol        (..)
        , Builtin       (..)
        , Literal       (..)

          -- ** Description
        , describeToken
        , describeTokenMeta
        , describeTokenAtom
        , describeTokenNamed
        , sayKeyword
        , saySymbol
        , sayBuiltin

          -- ** Renaming
        , renameToken

          -- ** Predicates
        , isVarName
        , isVarStart
        , isVarBody

        , isConName
        , isConStart
        , isConBody

        , isLitName
        , isLitStart
        , isLitBody

          -- ** Literal Reading
        , readLitInteger
        , readLitNat
        , readLitInt
        , readLitSize
        , readLitWordOfBits
        , readLitFloatOfBits
        , readBinary
        , readHex)
where
import DDC.Data.SourcePos
import DDC.Core.Codec.Text.Lexer.Token.Symbol
import DDC.Core.Codec.Text.Lexer.Token.Keyword
import DDC.Core.Codec.Text.Lexer.Token.Builtin
import DDC.Core.Codec.Text.Lexer.Token.Literal
import DDC.Core.Codec.Text.Lexer.Token.Names
import DDC.Core.Codec.Text.Pretty
import Data.Text                (Text)
import qualified Data.Text      as T


-- TokenFamily ----------------------------------------------------------------
-- | The family of a token.
--   This is used to help generate parser error messages,
--   so we can say ''the constructor Cons''
--             and ''the keyword case'' etc.
data TokenFamily
        = Symbol
        | Keyword
        | Constructor
        | Index
        | Literal
        | Pragma


-- | Describe a token family, for parser error messages.
describeTokenFamily :: TokenFamily -> String
describeTokenFamily tf
 = case tf of
        Symbol          -> "symbol"
        Keyword         -> "keyword"
        Constructor     -> "constructor"
        Index           -> "index"
        Literal         -> "literal"
        Pragma          -> "pragma"


-- Token ------------------------------------------------------------------------
-- | Tokens accepted by the core language parser.
data Token n
        -- | Some junk symbol that isn't part of the language.
        = KErrorJunk   String

        -- | The first part of an unterminated string.
        | KErrorUnterm String

        -- | Meta tokens contain out-of-band information that is eliminated
        --   before parsing proper.
        | KM    !TokenMeta

        -- | Atomic tokens are keywords, punctuation and baked-in
        --   constructor names.
        | KA    !TokenAtom

        -- | A named token that is specific to the language fragment
        --   (maybe it's a primop), or a user defined name.
        | KN    !(TokenNamed n)
        deriving (Eq, Show)


-- | Apply a function to all the names in a `Tok`.
renameToken
        :: Ord n2
        => (n1 -> Maybe n2)
        -> Token n1
        -> Maybe (Token n2)

renameToken f kk
 = case kk of
        KErrorJunk s
         -> Just $ KErrorJunk s

        KErrorUnterm s
          -> Just $ KErrorUnterm s

        KM t    -> Just $ KM t
        KA t    -> Just $ KA t
        KN t    -> fmap KN $ renameTokenNamed f t


-- | Describe a token for parser error messages.
describeToken :: Pretty n => Token n -> String
describeToken kk
 = case kk of
        KErrorJunk c    -> "character " ++ show c
        KErrorUnterm _  -> "unterminated string"
        KM tm           -> describeTokenMeta  tm
        KA ta           -> describeTokenAtom  ta
        KN tn           -> describeTokenNamed tn


-- TokMeta --------------------------------------------------------------------
-- | Meta tokens contain out-of-band information that is
--   eliminated before parsing proper.
data TokenMeta
        = KNewLine

        -- | Comment string.
        | KComment String

        -- | This is injected by `dropCommentBlock` when it finds
        --   an unterminated block comment.
        | KCommentUnterminated

        -- | This is injected by `applyOffside` when it finds an explit close
        --   brace in a position where it would close a synthetic one.
        | KOffsideClosingBrace
        deriving (Eq, Show)


-- | Describe a TokMeta, for lexer error messages.
describeTokenMeta :: TokenMeta -> String
describeTokenMeta tm
 = case tm of
        KNewLine                -> "new line"
        KComment{}              -> "comment"
        KCommentUnterminated    -> "unterminated block comment"
        KOffsideClosingBrace    -> "closing brace"


-- TokAtom --------------------------------------------------------------------
-- | Atomic tokens are keywords, punctuation and baked-in constructor names.
--   They don't contain user-defined names or primops specific to the
--   language fragment.
data TokenAtom
        -- | Pragmas.
        = KPragma  Text

        -- | Symbols.
        | KSymbol  Symbol

        -- | Keywords.
        | KKeyword Keyword

        -- | Builtin names.
        | KBuiltin Builtin

        -- | Infix operators, like in 1 + 2.
        | KOp      String

        -- | Wrapped operator, like in (+) 1 2.
        | KOpVar   String

        -- | Debrujn indices.
        | KIndex   Int

        -- | Literal values.
        | KLiteral
                Literal         -- Literal value.
                Bool            -- Trailing '#' prim specifier.
        deriving (Eq, Show)


-- | Describe a `TokAtom`, for parser error messages.
describeTokenAtom  :: TokenAtom -> String
describeTokenAtom ta
 = let  (family, str)           = describeTokenAtom' ta
   in   describeTokenFamily family ++ " " ++ show str

describeTokenAtom' :: TokenAtom -> (TokenFamily, String)
describeTokenAtom' ta
 = case ta of
        KPragma p       -> (Pragma,  "{-#" ++ T.unpack p ++ "#-}")
        KSymbol  ss     -> (Symbol,      saySymbol ss)
        KKeyword kw     -> (Keyword,     sayKeyword kw)
        KBuiltin bb     -> (Constructor, sayBuiltin bb)
        KOp      op     -> (Symbol, op)
        KOpVar   op     -> (Symbol, "(" ++ op ++ ")")
        KIndex   i      -> (Index,   "^" ++ show i)
        KLiteral l b    -> (Literal, show (l, b))


-- TokNamed -------------------------------------------------------------------
-- | A token with a user-defined name.
data TokenNamed n
        = KCon n
        | KVar n
        deriving (Eq, Show)


-- | Describe a `TokNamed`, for parser error messages.
describeTokenNamed :: Pretty n => TokenNamed n -> String
describeTokenNamed tn
 = case tn of
        KCon n  -> renderPlain $ text "constructor" %% (dquotes $ ppr n)
        KVar n  -> renderPlain $ text "variable"    %% (dquotes $ ppr n)


-- | Apply a function to all the names in a `TokNamed`.
renameTokenNamed
        :: Ord n2
        => (n1 -> Maybe n2)
        -> TokenNamed n1
        -> Maybe (TokenNamed n2)

renameTokenNamed f kk
  = case kk of
        KCon c  -> fmap KCon $ f c
        KVar c  -> fmap KVar $ f c

