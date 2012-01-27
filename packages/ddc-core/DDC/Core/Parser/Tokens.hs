
module DDC.Core.Parser.Tokens
        ( Tok      (..)
        , describeTok

        , TokAtom  (..)
        , describeTokAtom

        , TokNamed (..)
        , describeTokNamed)
where
import DDC.Core.Pretty
import DDC.Core.Exp
import DDC.Type.Transform.Rename


-- TokenFamily ----------------------------------------------------------------
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
--
--   * This is a conservative extension of the type tokens, 
--     as core expressions contain types.
--
data Tok n
        -- some junk that corresponds to a lexer error
        = KJunk Char
        | KA    !TokAtom 
        | KN    !(TokNamed n)
        deriving (Eq, Show)

        
describeTok :: Pretty n => Tok n -> String
describeTok kk
 = case kk of
        KJunk c         -> "character " ++ show c
        KA ta           -> describeTokAtom  ta
        KN tn           -> describeTokNamed tn


instance Rename Tok where
 rename f kk
  = case kk of
        KJunk s -> KJunk s
        KA t    -> KA t
        KN t    -> KN $ rename f t


-- TokAtom --------------------------------------------------------------------
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
        | KArrowEquals

        -- bottoms
        | KBotEffect
        | KBotClosure

        -- expression keywords
        | KIn
        | KOf
        | KLet
        | KLazy
        | KLetRec
        | KLetRegion
        | KWithRegion
        | KCase
        | KPurify
        | KForget
        | KWith
        | KWhere
        
        -- debruijn indices
        | KIndex Int

        -- builtin names
        | KTwConBuiltin TwCon
        | KWiConBuiltin WiCon
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
        KArrowEquals            -> (Constructor, "=>")

        -- bottoms
        KBotEffect              -> (Constructor, "!0")
        KBotClosure             -> (Constructor, "!$")

        -- expression keywords
        KIn                     -> (Keyword, "in")
        KOf                     -> (Keyword, "of")
        KLet                    -> (Keyword, "let")
        KLazy                   -> (Keyword, "lazy")
        KLetRec                 -> (Keyword, "letrec")
        KLetRegion              -> (Keyword, "letregion")
        KWithRegion             -> (Keyword, "withregion")
        KCase                   -> (Keyword, "case")
        KPurify                 -> (Keyword, "purify")
        KForget                 -> (Keyword, "forget")
        KWith                   -> (Keyword, "with")
        KWhere                  -> (Keyword, "where")
        
        -- debruijn indices
        KIndex i                -> (Index,   "^" ++ show i)

        -- builtin names
        KTwConBuiltin tw        -> (Constructor, pretty $ ppr tw)
        KWiConBuiltin wi        -> (Constructor, pretty $ ppr wi)
        KTcConBuiltin tc        -> (Constructor, pretty $ ppr tc)


-- TokNamed -------------------------------------------------------------------
data TokNamed n
        = KCon n
        | KVar n
        | KLit n
        deriving (Eq, Show)


-- | Describe a `TokNamed`, for parser error messages.
describeTokNamed :: Pretty n => TokNamed n -> String
describeTokNamed tn
 = case tn of
        KCon n  -> pretty $ text "constructor" <+> (dquotes $ ppr n)
        KVar n  -> pretty $ text "variable"    <+> (dquotes $ ppr n)
        KLit n  -> pretty $ text "literal"     <+> (dquotes $ ppr n)


instance Rename TokNamed where
 rename f kk
  = case kk of
        KCon c           -> KCon $ f c
        KVar c           -> KVar $ f c
        KLit c           -> KLit $ f c

