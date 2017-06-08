
module DDC.Core.Lexer.Token.Operator
        ( scanPrefixOperator
        , scanInfixOperator
        , acceptInfixOperator
        , isOpName
        , isOpStart
        , isOpBody)
where
import Text.Lexer.Inchworm.Char
import DDC.Core.Lexer.Unicode
import qualified Data.Set       as Set
import qualified Data.List      as List


-------------------------------------------------------------------------------
-- | Scanner for operators used prefix.
scanPrefixOperator :: Scanner IO Location [Char] (Location, String)
scanPrefixOperator
 = munchPred Nothing matchPrefixOperator acceptPrefixOperator


-- | Patch a prefix operator name.
matchPrefixOperator :: Int -> Char -> Bool
matchPrefixOperator 0 c         = c == '('
matchPrefixOperator _ c         = isOpBody c || c == ')'


-- | Accept a prefix operator name.
acceptPrefixOperator :: String -> Maybe String
acceptPrefixOperator str
        | '(' : cs1     <- str
        , c   : cs2     <- cs1
        , isOpStart c
        , (body , cs3)  <- List.span isOpBody cs2
        , ')' : []      <- cs3
        , not $ null (c : body)
        = Just (c : body)

        | otherwise
        = Nothing



-------------------------------------------------------------------------------
-- | Scanner for operators used infix.
scanInfixOperator  :: Scanner IO Location [Char] (Location, String)
scanInfixOperator
 = munchPred Nothing matchInfixOperator acceptInfixOperator


-- | Match an operator name.
matchInfixOperator  :: Int -> Char -> Bool
matchInfixOperator 0 c       = isOpStart c
matchInfixOperator _ c       = isOpBody  c


-- | Accept an operator name.
acceptInfixOperator :: String -> Maybe String
acceptInfixOperator str
 = case str of
        "="     -> Nothing
        "|"     -> Nothing
        "~>"    -> Nothing
        "->"    -> Nothing
        "<-"    -> Nothing
        "=>"    -> Nothing
        "/\\"   -> Nothing
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
        =  c == '~'     || c == '!'
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
        || c == '\\'
        || Set.member c unicodeOperatorsInfix




