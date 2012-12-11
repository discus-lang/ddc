
-- | Command-line interface to the DDC type checker, which is also 
--   possible via the plain 'ddc' command.
--
--   This code is provided as an example of how to drive the DDC
--   compiler framework.
--
module Main where
import Config
import DDC.Build.Language
import DDC.Build.Pipeline
import DDC.Core.Pretty
import System.IO
import System.Environment


main :: IO ()
main 
 = do   args    <- getArgs
        config  <- parseArgs args defaultConfig

        -- Get the source text.
        --  either from a file or stdin
        (source, sourceName)
          <- case configSourceFile config of
                Just fileName   
                 -> do  src     <- readFile fileName
                        return  (src, fileName)

                Nothing 
                 -> do  src     <- hGetContents stdin
                        return  (src, "<stdin>")

        -- Check with the configured language fragment.
        runLanguage config source sourceName (configLanguage config)


runLanguage config source sourceName language
 | Language bundle      <- language
 , fragment             <- bundleFragment bundle
 = do   
        -- In quiet mode just drop the checked module on the floor.
        let sink
                | configQuiet config    = SinkDiscard
                | otherwise             = SinkStdout

        -- Loading the core code automatically check it
        -- against the provided fragment.
        errs    <- pipeText sourceName 1 source
                $  PipeTextLoadCore  fragment
                [  PipeCoreOutput    sink ]

        -- If the pipeline died with errors, 
        --  then print them.
        mapM_ (putStrLn . renderIndent . ppr) errs

