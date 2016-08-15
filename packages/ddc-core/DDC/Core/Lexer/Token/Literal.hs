
module DDC.Core.Lexer.Token.Literal
        ( isLitName
        , isLitStart
        , isLitBody)
where
import qualified Data.Char      as Char


-- | String is the name of a literal.
isLitName :: String -> Bool
isLitName str
 = case str of
        []      -> False
        c : cs
         | isLitStart c
         , and (map isLitBody cs)
         -> True

         | otherwise
         -> False


-- | Character can start a literal.
isLitStart :: Char -> Bool
isLitStart c
        =   Char.isDigit c
        ||  c == '-'


-- | Character can be part of a literal body.
isLitBody :: Char -> Bool
isLitBody c
        =  Char.isDigit c
        || c == 'b' || c == 'o' || c == 'x'
        || c == 'w' || c == 'f' || c == 'i' || c == 's'
        || c == '.'
        || c == '#'
        || c == '\''

