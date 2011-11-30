
module DDC.Core.Parser.Tokens
        (Tok    (..))
where
import DDC.Type.Exp
import DDC.Core.Exp


-- | Tokens accepted by the core language parser.
--
--   * This is a conservative extension of the type tokens, 
--     as core expressions contain types.
--
data Tok n
        -- some junk that corresponds to a lexer error
        = KJunk String

        -- parens
        | KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KBraceBra
        | KBraceKet

        -- punctuation
        | KColon
        | KDot
        | KComma
        | KPlus
        | KBackSlash
        | KSemiColon

        -- symbolic constructors
        | KSortComp
        | KSortProp
        | KKindValue
        | KKindRegion
        | KKindEffect
        | KKindClosure
        | KKindWitness
        | KKindFun
        | KTypeFun
        | KTypeFunBra
        | KTypeFunKet

        -- bottoms
        | KBotEffect
        | KBotClosure

        -- expression keywords
        | KLetRec
        | KLet
        | KLocal
        | KIn
        | KCase
        | KOf
        | KPurify
        | KForget
        
        -- witness 
        | KTwConBuiltin TwCon
        | KTcConBuiltin (TcCon n)
        | KWiConBuiltin WiCon
        | KCon        n      
        | KVar        n
        deriving (Eq, Show)


