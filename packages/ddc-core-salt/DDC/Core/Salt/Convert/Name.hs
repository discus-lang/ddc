
module DDC.Core.Salt.Convert.Name
        ( sanitizeName
        , sanitizeGlobal
        , seaNameOfLocal)
where
import DDC.Core.Salt.Name
import DDC.Base.Pretty
import Data.Maybe



-- | Like 'sanitizeGlobal' but indicate that the name is going to be visible
--   globally.
sanitizeGlobal :: String -> String
sanitizeGlobal = sanitizeName
        

-- | Convert a Salt name to a string we can use for a local variable in the 
--   body of a C function.
seaNameOfLocal :: Name -> Maybe Doc
seaNameOfLocal nn
 = case nn of
        NameVar str     -> Just $ text $ "_" ++ sanitizeGlobal str
        _               -> Nothing


-- Sanitize ---------------------------------------------------------------------------------------
-- | Rewrite a name to make it safe to export as an external C symbol.
--
--   Names containing unfriendly characters like '&' are prefixed with '_sym_'
--   and the '&' is replaced by 'ZAn'. Literal 'Z's such a name are doubled to 'ZZ'.
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

