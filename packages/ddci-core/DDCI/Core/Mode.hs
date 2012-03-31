
module DDCI.Core.Mode
        ( Mode(..)
        , parseMode)
where


-- | DDCI mode flags.
data Mode
        -- | Display the expression at each step in the evaluation.
        =  TraceEval

        -- | Display the store state at each step in the evaluation.
        |  TraceStore

        -- | Render expressions displayed to user using indenting.
        |  Indent

        -- | When pretty printing Sea modules, include the #includes etc
        --   needed for compilation.
        |  SeaPrelude
        deriving (Eq, Ord, Show)


-- | Parse a mode from a string.
parseMode :: String -> Maybe Mode
parseMode str
 = case str of
        "TraceEval"     -> Just TraceEval
        "TraceStore"    -> Just TraceStore
        "Indent"        -> Just Indent
        "SeaPrelude"    -> Just SeaPrelude
        _               -> Nothing

