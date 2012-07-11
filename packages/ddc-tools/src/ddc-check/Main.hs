
-- | Command-line interface to the DDC type checker.
--   Used to check core files from the command line.
--
module Main where
import Config
import DDC.Build.Pipeline
import DDC.Build.Language
import DDC.Core.Pretty
import System.IO
import System.Environment

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
        runLanguage source sourceName (configLanguage config)


runLanguage source sourceName language
 | Language fragment <- language
 = do   
        -- Loading the core code automatically check it
        -- against the provided fragment.
        errs    <- pipeText sourceName 1 source
                $  PipeTextLoadCore  fragment
                [  PipeCoreOutput    SinkStdout ]

        -- If the pipeline died with errors, 
        --  then print them.
        mapM_ (putStrLn . renderIndent . ppr) errs

