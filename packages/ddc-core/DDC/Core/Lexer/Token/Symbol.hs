
module DDC.Core.Lexer.Token.Symbol
        ( Symbol (..)
        , saySymbol
        , scanSymbol)
where
import Text.Lexer.Inchworm.Char


-------------------------------------------------------------------------------
-- | Symbol tokens.
data Symbol
        -- Single char parenthesis
        = SRoundBra             -- ^ Like '('
        | SRoundKet             -- ^ Like ')'
        | SSquareBra            -- ^ Like '['
        | SSquareKet            -- ^ Like ']'
        | SBraceBra             -- ^ Like '{'
        | SBraceKet             -- ^ Like '}'

        -- Compound parenthesis
        | SSquareColonBra       -- ^ Like '[:'
        | SSquareColonKet       -- ^ Like ':]'
        | SBraceColonBra        -- ^ Like '{:'
        | SBraceColonKet        -- ^ Like ':}'

        -- Compound symbols.
        | SLambda               -- ^ Like 'λ'
        | SBigLambda            -- ^ Like 'Λ'
        | SBigLambdaSlash       -- ^ Like '/\\'

        -- Other punctuation.
        | SAt                   -- ^ Like '@'
        | SHat                  -- ^ Like '^'
        | SDot                  -- ^ Like '.'
        | SBar                  -- ^ Like '|'
        | SComma                -- ^ Like ','
        | SEquals               -- ^ Like '='
        | SSemiColon            -- ^ Like ';'
        | SBackSlash            -- ^ Like '\\'
        | SUnderscore           -- ^ Like '_'
        deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Yield the string name of a symbol token.
saySymbol :: Symbol -> String
saySymbol pp
 = case pp of
        -- Single character symbols.
        SRoundBra               -> "("
        SRoundKet               -> ")"
        SSquareBra              -> "["
        SSquareKet              -> "]"
        SBraceBra               -> "{"
        SBraceKet               -> "}"
        
        -- Compound parenthesis.
        SSquareColonBra         -> "[:"
        SSquareColonKet         -> ":]"
        SBraceColonBra          -> "{:"
        SBraceColonKet          -> ":}"

        -- Compound symbos.
        SLambda                 -> "λ"
        SBigLambda              -> "Λ"
        SBigLambdaSlash         -> "/\\"

        -- Other punctuation.
        SAt                     -> "@"
        SHat                    -> "^"
        SDot                    -> "."
        SBar                    -> "|"
        SComma                  -> ","
        SEquals                 -> "="
        SSemiColon              -> ";"
        SBackSlash              -> "\\"
        SUnderscore             -> "_"


-------------------------------------------------------------------------------
-- | Scanner for a `Symbol`.
scanSymbol :: Scanner IO Location [Char] (Location, Symbol)
scanSymbol
 = alt  (munchPred (Just 2) matchSymbol2 acceptSymbol2)
        (from      acceptSymbol1)


-- | Match a potential symbol character.
matchSymbol2 :: Int -> Char -> Bool
matchSymbol2 0 c
 = case c of
        '['     -> True
        '{'     -> True
        ':'     -> True
        _       -> False

matchSymbol2 1 c
 = case c of
        ']'     -> True
        '}'     -> True
        ':'     -> True
        _       -> False

matchSymbol2 _ _
 = False


-- | Accept a double character symbol.
acceptSymbol2 :: String -> Maybe Symbol
acceptSymbol2 ss
 = case ss of
        "[:"    -> Just SSquareColonBra
        ":]"    -> Just SSquareColonKet
        "{:"    -> Just SBraceColonBra
        ":}"    -> Just SBraceColonKet
        _       -> Nothing


-- | Accept a single character symbol.
acceptSymbol1 :: Char -> Maybe Symbol
acceptSymbol1 c 
 = case c of
        '('     -> Just SRoundBra
        ')'     -> Just SRoundKet
        '['     -> Just SSquareBra
        ']'     -> Just SSquareKet
        '{'     -> Just SBraceBra
        '}'     -> Just SBraceKet
        'λ'     -> Just SLambda
        'Λ'     -> Just SBigLambda
        '\\'    -> Just SBackSlash
        '@'     -> Just SAt
        '^'     -> Just SHat
        '.'     -> Just SDot
        '|'     -> Just SBar
        ','     -> Just SComma
        '='     -> Just SEquals
        ';'     -> Just SSemiColon
        '_'     -> Just SUnderscore
        _       -> Nothing

