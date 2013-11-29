
module DDC.Core.Lexer.Tokens
        ( -- * Tokens
          Tok      (..)
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
        , describeTokNamed)
where
import DDC.Core.Pretty
import DDC.Core.Exp
import Control.Monad


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


-- | Describe a token family, for parser error messages.
describeTokenFamily :: TokenFamily -> String
describeTokenFamily tf
 = case tf of
        Symbol          -> "symbol"
        Keyword         -> "keyword"
        Constructor     -> "constructor"
        Index           -> "index"


-- Tok ------------------------------------------------------------------------
-- | Tokens accepted by the core language parser.
data Tok n
        -- | Some junk symbol that isn't part of the language.
        = KJunk String

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
        KJunk s -> Just $ KJunk s
        KM t    -> Just $ KM t
        KA t    -> Just $ KA t
        KN t    -> liftM KN $ renameTokNamed f t


-- | Describe a token for parser error messages.
describeTok :: Pretty n => Tok n -> String
describeTok kk
 = case kk of
        KJunk c         -> "character " ++ show c
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
        -----------------------------------------
        -- Parens
        = KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KBraceBra
        | KBraceKet

        -----------------------------------------
        -- Compound parens
        | KSquareColonBra
        | KSquareColonKet
        | KBraceColonBra
        | KBraceColonKet

        -----------------------------------------
        -- Operator symbols
        -- These can be used as part of an infix operator.
        | KOp     String        -- ^ Naked operator,   like in 1 + 2.
        | KOpVar  String        -- ^ Wrapped operator, like in (+) 1 2.
        
        -----------------------------------------
        -- Operator body symbols.
        --   These can be used in an operator body, but cannot start an operator.
        
        -- Allowing '^' to start an operator causes problems in type signatures.
        --   We end up lexing a '^^^' operator with  '[^^^ : Int]'
        --   and a '^:' operator in                  '[^:Int]'
        | KHat

        -----------------------------------------
        -- Punctuation symbols.
        --   These cannot be used as part of an infix operator.
        
        -- Allowing '.' in an operator conflicts with trailing big-lambdas.
        --   We end up lexing './' as an operator in '\(x:Int)./\(a : Data)'
        | KDot
        
        | KComma           
        | KSemiColon
        | KUnderscore
        | KBackSlash
        
        -----------------------------------------
        -- Compound symbols.
        | KColonColon
        | KBigLambda

        -----------------------------------------
        -- symbolic constructors
        | KArrowTilde
        | KArrowDash
        | KArrowDashLeft
        | KArrowEquals

        -----------------------------------------
        -- bottoms
        | KBotEffect
        | KBotClosure

        -----------------------------------------
        -- core keywords
        | KModule
        | KImports
        | KExports
        | KData
        | KWith
        | KWhere
        | KIn
        | KLet
        | KLetCase
        | KLetRec
        | KPrivate
        | KExtend
        | KUsing
        | KWithRegion
        | KCase
        | KOf
        | KType
        | KWeakEff
        | KWeakClo
        | KPurify
        | KForget
        | KBox
        | KRun

        -----------------------------------------
        -- sugar keywords
        | KDo
        | KMatch
        | KElse

        -----------------------------------------
        -- debruijn indices
        | KIndex Int

        -----------------------------------------
        -- builtin names 
        --   sort constructors.
        | KSoConBuiltin SoCon

        --   kind constructors.
        | KKiConBuiltin KiCon

        --   witness type constructors.
        | KTwConBuiltin TwCon

        --   witness constructors.
        | KWbConBuiltin WbCon

        --   other builtin spec constructors.
        | KTcConBuiltin TcCon

        --   the unit data constructor.
        | KDaConUnit
        deriving (Eq, Show)


-- | Describe a `TokAtom`, for parser error messages.
describeTokAtom  :: TokAtom -> String
describeTokAtom ta
 = let  (family, str)           = describeTokAtom' ta
   in   describeTokenFamily family ++ " " ++ show str

describeTokAtom' :: TokAtom -> (TokenFamily, String)
describeTokAtom' ta
 = case ta of
        -- parens
        KRoundBra               -> (Symbol, "(")
        KRoundKet               -> (Symbol, ")")
        KSquareBra              -> (Symbol, "[")
        KSquareKet              -> (Symbol, "]")
        KBraceBra               -> (Symbol, "{")
        KBraceKet               -> (Symbol, "}")
        
        -- compound parens
        KSquareColonBra         -> (Symbol, "[:")
        KSquareColonKet         -> (Symbol, ":]")
        KBraceColonBra          -> (Symbol, "{:")
        KBraceColonKet          -> (Symbol, ":}")

        -- operator symbols
        KOp    op               -> (Symbol, op)
        KOpVar op               -> (Symbol, "(" ++ op ++ ")")
        
        -- operator body symbols
        KHat                    -> (Symbol, "^")

        -- punctuation symbols
        KDot                    -> (Symbol, ".")
        KComma                  -> (Symbol, ",")
        KSemiColon              -> (Symbol, ";")
        KUnderscore             -> (Symbol, "_")
        KBackSlash              -> (Symbol, "\\")
        KColonColon             -> (Symbol, "::")
        KBigLambda              -> (Symbol, "/\\")

        -- symbolic constructors
        KArrowTilde             -> (Constructor, "~>")
        KArrowDash              -> (Constructor, "->")
        KArrowDashLeft          -> (Constructor, "<-")
        KArrowEquals            -> (Constructor, "=>")

        -- bottoms
        KBotEffect              -> (Constructor, "Pure")
        KBotClosure             -> (Constructor, "Empty")
        
        -- expression keywords
        KModule                 -> (Keyword, "module")
        KImports                -> (Keyword, "imports")
        KExports                -> (Keyword, "exports")
        KData                   -> (Keyword, "data")
        KWith                   -> (Keyword, "with")
        KWhere                  -> (Keyword, "where")
        KIn                     -> (Keyword, "in")
        KLet                    -> (Keyword, "let")
        KLetCase                -> (Keyword, "letcase")
        KLetRec                 -> (Keyword, "letrec")
        KPrivate                -> (Keyword, "private")
        KExtend                 -> (Keyword, "extend")
        KUsing                  -> (Keyword, "using")
        KWithRegion             -> (Keyword, "withregion")
        KCase                   -> (Keyword, "case")
        KOf                     -> (Keyword, "of")
        KType                   -> (Keyword, "type")
        KWeakEff                -> (Keyword, "weakeff")
        KWeakClo                -> (Keyword, "weakclo")
        KPurify                 -> (Keyword, "purify")
        KForget                 -> (Keyword, "forget")
        KBox                    -> (Keyword, "box")
        KRun                    -> (Keyword, "run")

        -- sugar keywords
        KDo                     -> (Keyword, "do")
        KMatch                  -> (Keyword, "match")
        KElse                   -> (Keyword, "else")

        -- debruijn indices
        KIndex i                -> (Index,   "^" ++ show i)

        -- builtin names
        KSoConBuiltin so        -> (Constructor, renderPlain $ ppr so)
        KKiConBuiltin ki        -> (Constructor, renderPlain $ ppr ki)
        KTwConBuiltin tw        -> (Constructor, renderPlain $ ppr tw)
        KWbConBuiltin wi        -> (Constructor, renderPlain $ ppr wi)
        KTcConBuiltin tc        -> (Constructor, renderPlain $ ppr tc)
        KDaConUnit              -> (Constructor, "()")
        

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

