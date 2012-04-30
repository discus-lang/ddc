
module DDCI.Core.Mode
        ( Mode(..)
        , readMode

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

        -- | When pretty printing Salt modules as C code,
        --  include the #includes etc needed for compilation.
        |  SaltPrelude
        deriving (Eq, Ord, Show)


-- | Parse a mode from a string.
readMode :: String -> Maybe Mode
readMode str
 = case str of
        "TraceEval"     -> Just TraceEval
        "TraceStore"    -> Just TraceStore
        "Indent"        -> Just Indent
        "SaltPrelude"   -> Just SaltPrelude
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
        SourceConsole{}         -> "<console>"
        SourceBatch{}           -> "<batch>"
