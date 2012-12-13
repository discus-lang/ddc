
module DDC.Core.Simplifier.Lexer
        ( Tok(..)
        , lexSimplifier)
where
import DDC.Data.Token
import DDC.Data.SourcePos
import Data.Char


lexSimplifier :: String -> [Token Tok]
lexSimplifier str
 = map (\t -> Token t (SourcePos "<simplifier spec>" 0 0)) 
 $ lexer str


-- Lexer ----------------------------------------------------------------------
-- | Lex a transform specification.
lexer :: String -> [Tok]
lexer ss
 = case ss of
        []              -> []

        (';' : cs)
         -> KSemi      : lexer cs

        ('{' : cs)
         -> KBraceBra  : lexer cs

        ('}' : cs)
         -> KBraceKet  : lexer cs

        ('[' : cs)
         -> KSquareBra : lexer cs

        (']' : cs)
         -> KSquareKet : lexer cs

        ('-' : cs)
         -> KMinus     : lexer cs

        ('+' : cs)
         -> KPlus      : lexer cs

        (c : cs)
         |  isSpace c
         -> lexer cs

        (c : cs)
         | isUpper c
         , (body, rest) <- span isAlpha cs
         -> KTrans  (c : body) : lexer rest

        (c : cs)
         | isLower c
         ,  (body, rest) <- span isAlpha cs
         -> KVar    (c : body) : lexer rest

        (c : cs)
         | isDigit c
         , (digits, rest) <- span isDigit cs
         -> KInt (read (c:digits)) : lexer rest

        _ -> [KJunk ss]


-- | Tokens for transform specification.
data Tok
        = KJunk         String
        | KTrans        String
        | KVar          String
        | KInt          Int

        | KSemi
        | KComma
        | KMinus
        | KPlus
        | KBraceBra
        | KBraceKet
        | KSquareBra
        | KSquareKet
        deriving (Eq, Show)
