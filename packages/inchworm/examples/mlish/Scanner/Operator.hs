
module Scanner.Operator
        (scanOperator)
where
import Token
import Text.Lexer.Inchworm.Char


-- | Scan an infix operator.
scanOperator :: Scanner IO Location [Char] (Location, Token)
scanOperator 
 = munchPred Nothing matchOp acceptOp
 where
        matchOp 0 c     = isOpStart c
        matchOp _ c     = isOpBody  c
        acceptOp cs     = Just $ KOp cs


-- | Character can start an operator.
isOpStart :: Char -> Bool
isOpStart c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'                     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'                     || c == '/'     || c == '|'
        || c == '<'     || c == '>'


-- | Character can be part of an operator body.
isOpBody :: Char -> Bool
isOpBody c
        =  c == '~'     || c == '!'                     || c == '#'     
        || c == '$'     || c == '%'     || c == '^'     || c == '&'     
        || c == '*'     || c == '-'     || c == '+'     || c == '='
        || c == ':'     || c == '?'     || c == '/'     || c == '|'
        || c == '<'     || c == '>'
