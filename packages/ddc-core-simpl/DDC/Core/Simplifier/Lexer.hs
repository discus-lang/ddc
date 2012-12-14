
module DDC.Core.Simplifier.Lexer
        ( Tok(..)
        , lexSimplifier)
where
import DDC.Data.Token
import DDC.Data.SourcePos
import Data.Char


lexSimplifier 
        :: (String -> Maybe n)  -- ^ Function to read a name.
        -> String               -- ^ String to parse.
        -> [Token (Tok n)]

lexSimplifier readName str
 = map (\t -> Token t (SourcePos "<simplifier spec>" 0 0)) 
 $ lexer readName str


-- Lexer ----------------------------------------------------------------------
-- | Lex a transform specification.
lexer   :: (String -> Maybe n)  -- ^ Function to read a name.
        -> String               -- ^ String to parse.
        -> [Tok n]

lexer readName ss
 = let down = lexer readName
   in case ss of
        []              -> []

        (';' : cs)      -> KSemiColon : down cs
        (',' : cs)      -> KComma     : down cs
        ('-' : cs)      -> KMinus     : down cs
        ('+' : cs)      -> KPlus      : down cs
        ('{' : cs)      -> KBraceBra  : down cs
        ('}' : cs)      -> KBraceKet  : down cs
        ('[' : cs)      -> KSquareBra : down cs
        (']' : cs)      -> KSquareKet : down cs

        (c : cs)
         |  isSpace c
         -> down cs

        (c : cs)
         | isUpper c
         , (body, rest) <- span isAlpha cs
         -> KCon    (c : body) : down rest

        (c : cs)
         | isLower c
         , (body, rest) <- span isAlpha cs
         , Just n       <- readName (c : body)
         -> KVar  n : down rest

        (c : cs)
         | isDigit c
         , (digits, rest) <- span isDigit cs
         -> KInt (read (c:digits)) : down rest

        _ -> [KJunk ss]


-- | Tokens for transform specification.
data Tok n
        = KEnd
        | KJunk  String
        | KCon   String
        | KVar   n
        | KInt   Int

        | KSemiColon
        | KComma
        | KMinus
        | KPlus
        | KBraceBra
        | KBraceKet
        | KSquareBra
        | KSquareKet
        deriving (Eq, Show)
