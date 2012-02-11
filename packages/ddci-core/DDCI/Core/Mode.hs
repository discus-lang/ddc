
module DDCI.Core.Mode
        ( Mode(..)
        , parseMode)
where


-- | Interpreter mode flags.
data Mode
        -- | Display the expression at each step in the evaluation.
        =  TraceEval

        -- | Display the store state at each step in the evaluation.
        |  TraceStore

        -- | Render expressions displayed to user using indenting.
        |  Indent
        deriving (Eq, Ord, Show)


-- | Parse a mode from a string.
parseMode :: String -> Maybe Mode
parseMode str
 = case str of
        "TraceEval"     -> Just TraceEval
        "TraceStore"    -> Just TraceStore
        "Indent"        -> Just Indent
        _               -> Nothing

