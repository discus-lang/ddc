
module DDC.Core.Lexer.Names
        ( -- * Literal names
          isLitName
        , isLitStart
        , isLitBody)
where
import Data.Char



-- Literal names ----------------------------------------------------------------------------------
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
        =   isDigit c
        ||  c == '-'

-- | Character can be part of a literal body.
isLitBody :: Char -> Bool
isLitBody c
        =  isDigit c
        || c == 'b' || c == 'o' || c == 'x'
        || c == 'w' || c == 'f' || c == 'i' || c == 's'
        || c == '.'
        || c == '#'
        || c == '\''

