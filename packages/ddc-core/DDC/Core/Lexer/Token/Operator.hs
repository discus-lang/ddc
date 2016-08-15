
module DDC.Core.Lexer.Token.Operator
        ( scanOperator
        , acceptOperator
        , isOpName
        , isOpStart
        , isOpBody)
where
import Text.Lexer.Inchworm.Char
import DDC.Core.Lexer.Unicode
import qualified Data.Set       as Set


-- | Scanner for operators.
scanOperator :: Scanner IO Location [Char] (Location, String)
scanOperator 
 = munchPred Nothing matchOperator acceptOperator


-- | Match an operator name.
matchOperator  :: Int -> Char -> Bool
matchOperator 0 c       = isOpStart c
matchOperator _ c       = isOpBody  c


-- | Accept an operator name.
acceptOperator :: String -> Maybe String
acceptOperator str
 = case str of
        "="     -> Nothing
        "|"     -> Nothing
        _       -> Just str


-- | String is the name of some operator.
isOpName :: String -> Bool
isOpName str
 = case str of
        []      -> False
        c : cs
         | isOpStart c
         , and (map isOpBody cs)
         -> True

         | otherwise
         -> False


-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'
        || Set.member c unicodeOperatorsInfix


-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'
        || Set.member c unicodeOperatorsInfix




