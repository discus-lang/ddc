
module DDC.Driver.Source
        ( Source(..)
        , lineStartOfSource
        , nameOfSource)
where

-- | Where some source code was obtained from.
--
--   This is used when generating error messages.
data Source
        -- | Read directly from a file.
        = SourceFile            FilePath 

        -- | Supplied on the command line.
        | SourceArgs            

        -- | Typed into the console.
        | SourceConsole         Int

        -- | Part of a @.dcx@ batch file.
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
