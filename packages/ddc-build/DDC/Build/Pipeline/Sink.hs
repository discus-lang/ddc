
module DDC.Build.Pipeline.Sink
        ( Sink(..)
        , pipeSink)
where
import DDC.Build.Pipeline.Error

-- | What to do with program text.
data Sink
        -- | Drop it on the floor.
        = SinkDiscard

        -- | Emit it to stdout.
        | SinkStdout

        -- | Write it to this file.
        | SinkFile FilePath
        deriving (Show)


-- | Emit a string to the given `Sink`.
pipeSink :: String -> Sink -> IO [Error]
pipeSink !str !tg
 = case tg of
        SinkDiscard
         -> do  return []

        SinkStdout
         -> do  putStrLn str
                return []

        SinkFile path
         -> do  writeFile path str
                return []
