
module DDCI.Core.Mode
        ( Mode(..)
        , parseMode

        , Source        (..)
        , lineStartOfSource
        , nameOfSource)
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


-- | The source of some program text.
--   This is different from 'Input', because an interactive command may instruct us
--   to load some program text from a file.
data Source
        = SourceFile            FilePath 
        | SourceArgs            
        | SourceConsole         Int
        | SourceBatch           FilePath Int
        deriving (Eq, Show)


-- | Get the starting source line number to report for this source.
lineStartOfSource :: Source -> Int
lineStartOfSource ss
 = case ss of
        SourceFile{}            -> 1
        SourceArgs{}            -> 1
        SourceConsole i         -> i
        SourceBatch _ i         -> i


-- | Get the name of a source.
nameOfSource :: Source -> String
nameOfSource ss
 = case ss of
        SourceFile f            -> f
        SourceArgs              -> "<arg>"
        SourceConsole{}         -> "<sdfdsf>"
        SourceBatch{}           -> "<batch>"


