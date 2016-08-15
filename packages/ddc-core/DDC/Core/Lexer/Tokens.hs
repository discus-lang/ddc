
module DDC.Core.Lexer.Tokens
        ( Located (..)
        , columnOfLocated

          -- * Tokens
        , Tok      (..)
        , renameTok
        , describeTok

          -- * Meta Tokens
        , TokMeta  (..)
        , describeTokMeta

          -- * Atomic Tokens
        , TokAtom  (..)
        , describeTokAtom

          -- * Named Tokens
        , TokNamed (..)
        , describeTokNamed

          -- * Keywords
        , Keyword  (..),        sayKeyword
        , Symbol   (..),        saySymbol
        , Builtin  (..),        sayBuiltin)
where
import DDC.Data.SourcePos
import DDC.Core.Lexer.Token.Symbol
import DDC.Core.Lexer.Token.Keyword
import DDC.Core.Lexer.Token.Builtin
import DDC.Core.Pretty
import Control.Monad
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


-- Tok ------------------------------------------------------------------------
-- | Tokens accepted by the core language parser.
data Tok n
        -- | Some junk symbol that isn't part of the language.
        = KErrorJunk String

        -- | The first part of an unterminated string.
        | KErrorUnterm String

        -- | Meta tokens contain out-of-band information that is eliminated
        --   before parsing proper.
        | KM    !TokMeta

        -- | Atomic tokens are keywords, punctuation and baked-in 
        --   constructor names.
        | KA    !TokAtom 

        -- | A named token that is specific to the language fragment 
        --   (maybe it's a primop), or a user defined name.
        | KN    !(TokNamed n)
        deriving (Eq, Show)


-- | Apply a function to all the names in a `Tok`.
renameTok
        :: Ord n2
        => (n1 -> Maybe n2) 
        -> Tok n1 
        -> Maybe (Tok n2)

renameTok f kk
 = case kk of
        KErrorJunk s 
         -> Just $ KErrorJunk s

        KErrorUnterm s
          -> Just $ KErrorUnterm s

        KM t    -> Just $ KM t
        KA t    -> Just $ KA t
        KN t    -> liftM KN $ renameTokNamed f t


-- | Describe a token for parser error messages.
describeTok :: Pretty n => Tok n -> String
describeTok kk
 = case kk of
        KErrorJunk c    -> "character " ++ show c
        KErrorUnterm _  -> "unterminated string"
        KM tm           -> describeTokMeta  tm
        KA ta           -> describeTokAtom  ta
        KN tn           -> describeTokNamed tn


-- TokMeta --------------------------------------------------------------------
-- | Meta tokens contain out-of-band information that is 
--   eliminated before parsing proper.
data TokMeta
        = KNewLine
        | KCommentLineStart
        | KCommentBlockStart
        | KCommentBlockEnd

        -- | This is injected by `dropCommentBlock` when it finds
        --   an unterminated block comment.
        | KCommentUnterminated

        -- | This is injected by `applyOffside` when it finds an explit close
        --   brace in a position where it would close a synthetic one.
        | KOffsideClosingBrace
        deriving (Eq, Show)


-- | Describe a TokMeta, for lexer error messages.
describeTokMeta :: TokMeta -> String
describeTokMeta tm
 = case tm of
        KNewLine                -> "new line"
        KCommentLineStart       -> "comment start"
        KCommentBlockStart      -> "block comment start"
        KCommentBlockEnd        -> "block comment end"
        KCommentUnterminated    -> "unterminated block comment"
        KOffsideClosingBrace    -> "closing brace"


-- TokAtom --------------------------------------------------------------------
-- | Atomic tokens are keywords, punctuation and baked-in constructor names.
--   They don't contain user-defined names or primops specific to the 
--   language fragment.
data TokAtom
        = KPragma  Text         -- ^ Pragmas.
        | KKeyword Keyword      -- ^ Keywords.
        | KSymbol  Symbol       -- ^ Symbols.
        | KBuiltin Builtin      -- ^ Built in names.
        | KOp      String       -- ^ Naked operator,   like in 1 + 2.
        | KOpVar   String       -- ^ Wrapped operator, like in (+) 1 2.
        | KIndex   Int          -- ^ Debruijn indices.
        | KString  Text         -- ^ Literal strings.
        deriving (Eq, Show)


-- | Describe a `TokAtom`, for parser error messages.
describeTokAtom  :: TokAtom -> String
describeTokAtom ta
 = let  (family, str)           = describeTokAtom' ta
   in   describeTokenFamily family ++ " " ++ show str

describeTokAtom' :: TokAtom -> (TokenFamily, String)
describeTokAtom' ta
 = case ta of
        KSymbol  ss             -> (Symbol,      saySymbol ss)
        KKeyword kw             -> (Keyword,     sayKeyword kw)
        KBuiltin bb             -> (Constructor, sayBuiltin bb)
        KOp    op               -> (Symbol, op)
        KOpVar op               -> (Symbol, "(" ++ op ++ ")")
        KIndex  i               -> (Index,   "^" ++ show i)
        KString s               -> (Literal, show s)
        KPragma p               -> (Pragma,  "{-#" ++ T.unpack p ++ "#-}")
        

-- TokNamed -------------------------------------------------------------------
-- | A token with a user-defined name.
data TokNamed n
        = KCon n
        | KVar n
        | KLit n
        deriving (Eq, Show)


-- | Describe a `TokNamed`, for parser error messages.
describeTokNamed :: Pretty n => TokNamed n -> String
describeTokNamed tn
 = case tn of
        KCon n  -> renderPlain $ text "constructor" <+> (dquotes $ ppr n)
        KVar n  -> renderPlain $ text "variable"    <+> (dquotes $ ppr n)
        KLit n  -> renderPlain $ text "literal"     <+> (dquotes $ ppr n)


-- | Apply a function to all the names in a `TokNamed`.
renameTokNamed 
        :: Ord n2
        => (n1 -> Maybe n2) 
        -> TokNamed n1 
        -> Maybe (TokNamed n2)

renameTokNamed f kk
  = case kk of
        KCon c           -> liftM KCon $ f c
        KVar c           -> liftM KVar $ f c
        KLit c           -> liftM KLit $ f c

