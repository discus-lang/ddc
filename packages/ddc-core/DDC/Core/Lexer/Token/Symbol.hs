
module DDC.Core.Lexer.Token.Symbol
        ( Symbol (..)
        , saySymbol)
where


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
        | SLambda
        | SBigLambda
        | SBigLambdaSlash

        -- Other punctuation.
        | SAt
        | SHat
        | SDot
        | SBar
        | SComma           
        | SEquals
        | SSemiColon
        | SBackSlash
        | SUnderscore
        deriving (Eq, Show)


-- | Yield the name of a symbol token.
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

