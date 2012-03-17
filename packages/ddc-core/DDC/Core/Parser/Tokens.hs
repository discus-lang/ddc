
module DDC.Core.Parser.Tokens
        ( Tok      (..)
        , describeTok
        , renameTok

        , TokAtom  (..)
        , describeTokAtom

        , TokNamed (..)
        , describeTokNamed)
where
import DDC.Core.Pretty
import DDC.Core.Exp


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
        | Variable


-- | Describe a token family, for parser error messages.
describeTokenFamily :: TokenFamily -> String
describeTokenFamily tf
 = case tf of
        Symbol          -> "symbol"
        Keyword         -> "keyword"
        Constructor     -> "constructor"
        Index           -> "index"
        Variable        -> "variable"


-- Tok ------------------------------------------------------------------------
-- | Tokens accepted by the core language parser.
data Tok n
        -- Some junk symbol that isn't part of the language.
        = KJunk Char

        -- An atomic token.
        | KA    !TokAtom 

        -- A named token.
        | KN    !(TokNamed n)
        deriving (Eq, Show)


-- | Describe a token for parser error messages.
describeTok :: Pretty n => Tok n -> String
describeTok kk
 = case kk of
        KJunk c         -> "character " ++ show c
        KA ta           -> describeTokAtom  ta
        KN tn           -> describeTokNamed tn


-- | Apply a function to all the names in a `Tok`.
renameTok
        :: Ord n2
        => (n1 -> n2) -> Tok n1 -> Tok n2

renameTok f kk
 = case kk of
        KJunk s -> KJunk s
        KA t    -> KA t
        KN t    -> KN $ renameTokNamed f t


-- TokAtom --------------------------------------------------------------------
-- | Atomic tokens, that don't contain user-defined names.
data TokAtom
        -- parens
        = KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KBraceBra
        | KBraceKet
        | KAngleBra
        | KAngleKet

        -- compound parens
        | KSquareColonBra
        | KSquareColonKet
        | KAngleColonBra
        | KAngleColonKet

        -- punctuation
        | KDot
        | KBar
        | KHat
        | KPlus
        | KColon
        | KComma
        | KBackSlash
        | KSemiColon
        | KUnderscore
        | KEquals
        | KAmpersand
        | KDash
        | KColonColon
        | KBigLambda

        -- symbolic constructors
        | KSortComp
        | KSortProp
        | KKindValue
        | KKindRegion
        | KKindEffect
        | KKindClosure
        | KKindWitness
        | KArrowTilde
        | KArrowDash
        | KArrowDashLeft
        | KArrowEquals

        -- bottoms
        | KBotEffect
        | KBotClosure

        -- core keywords
        | KModule
        | KImports
        | KExports
        | KWith
        | KWhere
        | KIn
        | KLet
        | KLazy
        | KLetRec
        | KLetRegion
        | KWithRegion
        | KCase
        | KOf
        | KWeakEff
        | KWeakClo
        | KPurify
        | KForget

        -- sugar keywords
        | KMatch
        | KElse

        -- debruijn indices
        | KIndex Int

        -- builtin names
        | KTwConBuiltin TwCon
        | KWbConBuiltin WbCon
        | KTcConBuiltin TcCon
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
        KAngleBra               -> (Symbol, "<")
        KAngleKet               -> (Symbol, ">")

        -- compound parens
        KSquareColonBra         -> (Symbol, "[:")
        KSquareColonKet         -> (Symbol, ":]")
        KAngleColonBra          -> (Symbol, "<:")
        KAngleColonKet          -> (Symbol, ":>")

        -- punctuation
        KDot                    -> (Symbol, ".")
        KBar                    -> (Symbol, "|")
        KHat                    -> (Symbol, "^")
        KPlus                   -> (Symbol, "+")
        KColon                  -> (Symbol, ":")
        KComma                  -> (Symbol, ",")
        KBackSlash              -> (Symbol, "\\")
        KSemiColon              -> (Symbol, ";")
        KUnderscore             -> (Symbol, "_")
        KEquals                 -> (Symbol, "=")
        KAmpersand              -> (Symbol, "&")
        KDash                   -> (Symbol, "-")
        KColonColon             -> (Symbol, "::")
        KBigLambda              -> (Symbol, "/\\")

        -- symbolic constructors
        KSortComp               -> (Constructor, "**")
        KSortProp               -> (Constructor, "@@")
        KKindValue              -> (Constructor, "*")
        KKindRegion             -> (Constructor, "%")
        KKindEffect             -> (Constructor, "!")
        KKindClosure            -> (Constructor, "$")
        KKindWitness            -> (Constructor, "@")
        KArrowTilde             -> (Constructor, "~>")
        KArrowDash              -> (Constructor, "->")
        KArrowDashLeft          -> (Constructor, "<-")
        KArrowEquals            -> (Constructor, "=>")

        -- bottoms
        KBotEffect              -> (Constructor, "!0")
        KBotClosure             -> (Constructor, "!$")

        -- expression keywords
        KModule                 -> (Keyword, "module")
        KImports                -> (Keyword, "imports")
        KExports                -> (Keyword, "exports")
        KWith                   -> (Keyword, "with")
        KWhere                  -> (Keyword, "where")
        KIn                     -> (Keyword, "in")
        KLet                    -> (Keyword, "let")
        KLazy                   -> (Keyword, "lazy")
        KLetRec                 -> (Keyword, "letrec")
        KLetRegion              -> (Keyword, "letregion")
        KWithRegion             -> (Keyword, "withregion")
        KCase                   -> (Keyword, "case")
        KOf                     -> (Keyword, "of")
        KWeakEff                -> (Keyword, "weakeff")
        KWeakClo                -> (Keyword, "weakclo")
        KPurify                 -> (Keyword, "purify")
        KForget                 -> (Keyword, "forget")

        -- sugar keywords
        KMatch                  -> (Keyword, "match")
        KElse                   -> (Keyword, "else")

        -- debruijn indices
        KIndex i                -> (Index,   "^" ++ show i)

        -- builtin names
        KTwConBuiltin tw        -> (Constructor, renderPlain $ ppr tw)
        KWbConBuiltin wi        -> (Constructor, renderPlain $ ppr wi)
        KTcConBuiltin tc        -> (Constructor, renderPlain $ ppr tc)


-- TokNamed -------------------------------------------------------------------
-- | A token witn a user-defined name.
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
        => (n1 -> n2) -> TokNamed n1 -> TokNamed n2

renameTokNamed f kk
  = case kk of
        KCon c           -> KCon $ f c
        KVar c           -> KVar $ f c
        KLit c           -> KLit $ f c

