
module DDC.Driver.Source
        ( Source(..)
        , lineStartOfSource
        , nameOfSource)
where

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
