
module DDC.Type.Parser.Tokens
        (Tok    (..))
where
import DDC.Type.Exp


-- | Tokens accepted by the type parser. 
data Tok n
        = KJunk         String
        | KRoundBra
        | KRoundKet
        | KSquareBra
        | KSquareKet
        | KColon
        | KComma
        | KDot
        | KPlus
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
        | KBotEffect
        | KBotClosure
        | KTwConBuiltin  TwCon
        | KTcConBuiltin  (TcCon n)
        | KCon   n
        | KVar   n
        deriving (Eq, Show)

