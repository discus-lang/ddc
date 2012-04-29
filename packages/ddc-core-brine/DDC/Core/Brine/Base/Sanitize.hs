
module DDC.Core.Brine.Base.Sanitize
        (sanitizeName)
where
import Data.Maybe


-- | Rewrite the symbols in a name to make it safe to export as an external
--   symbol. Names containing symbols are prefixed with '_sym_' and a symbol
--   like '&' is replaced by 'ZAt'. Literal 'Z's in a symbolic name are
--   doubled.
--
sanitizeName :: String -> String
sanitizeName str
 = let  hasSymbols      = any isJust $ map convertSymbol str
   in   if hasSymbols
         then "_sym_" ++ concatMap rewriteChar str
         else str


-- | Get the encoded version of a character.
rewriteChar :: Char -> String
rewriteChar c
        | Just str <- convertSymbol c      = "Z" ++ str
        | 'Z'      <- c                    = "ZZ"
        | otherwise                        = [c]


-- | Convert symbols to their sanitized form.
convertSymbol :: Char -> Maybe String
convertSymbol c
 = case c of
        '!'     -> Just "Bg"
        '@'     -> Just "At"
        '#'     -> Just "Hs"
        '$'     -> Just "Dl"
        '%'     -> Just "Pc"
        '^'     -> Just "Ht"
        '&'     -> Just "An"
        '*'     -> Just "St"
        '~'     -> Just "Tl"
        '-'     -> Just "Ms"
        '+'     -> Just "Ps"
        '='     -> Just "Eq"
        '|'     -> Just "Pp"
        '\\'    -> Just "Bs"
        '/'     -> Just "Fs"
        ':'     -> Just "Cl"
        '.'     -> Just "Dt"
        '?'     -> Just "Qm"
        '<'     -> Just "Lt"
        '>'     -> Just "Gt"
        '['     -> Just "Br"
        ']'     -> Just "Kt"
        '\''    -> Just "Pm"
        '`'     -> Just "Bt"
        _       -> Nothing


