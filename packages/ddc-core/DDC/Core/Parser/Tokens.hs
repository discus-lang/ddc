
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


        -- compound parens
        | KBraceColonBra
        | KBraceColonKet
        | KAngleColonBra
        | KAngleColonKet

        -- parens
        | KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KBraceBra
        | KBraceKet
        | KAngleBra
        | KAngleKet

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

        -- expression keywords
        | KIn
        | KOf
        | KLet
        | KLetRec
        | KLocal
        | KCase
        | KPurify
        | KForget

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
        
        -- named things
        | KTwConBuiltin TwCon
        | KTcConBuiltin (TcCon n)
        | KWiConBuiltin WiCon
        | KCon        n      
        | KVar        n
        
        -- literal values
        | KInteger Integer
        deriving (Eq, Show)


