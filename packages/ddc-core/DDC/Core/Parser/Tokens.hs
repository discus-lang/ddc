
module DDC.Core.Parser.Tokens
        ( Tok      (..)
        , TokAtom  (..)
        , TokNamed (..))
where
import DDC.Type.Exp
import DDC.Core.Exp
import DDC.Type.Transform.Rename


-- | Tokens accepted by the core language parser.
--
--   * This is a conservative extension of the type tokens, 
--     as core expressions contain types.
--
data Tok n
        -- some junk that corresponds to a lexer error
        = KJunk String
        | KA !TokAtom 
        | KN !(TokNamed n)
        deriving (Eq, Show)
        

data TokAtom
        -- compound parens
        = KBraceColonBra
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

        -- compound symbols
        | KHasType

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
        | KRec
        | KRegion
        | KCase
        | KPurify
        | KForget
        | KWith
        | KWhere

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
        
        -- debruijn indices
        | KIndex Int

        -- builtin names
        | KTwConBuiltin TwCon
        | KWiConBuiltin WiCon
        | KTcConBuiltin TcCon
        deriving (Eq, Show)


data TokNamed n
        = KCon n
        | KVar n
        | KLit n
        deriving (Eq, Show)


instance Rename Tok where
 rename f kk
  = case kk of
        KJunk s -> KJunk s
        KA t    -> KA t
        KN t    -> KN $ rename f t


instance Rename TokNamed where
 rename f kk
  = case kk of
        KCon c           -> KCon $ f c
        KVar c           -> KVar $ f c
        KLit c           -> KLit $ f c

