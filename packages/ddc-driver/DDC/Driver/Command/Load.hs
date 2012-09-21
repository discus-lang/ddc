
module DDC.Driver.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDC.Driver.Source
import DDC.Driver.Bundle
import DDC.Build.Pipeline
import DDC.Core.Load
import DDC.Core.Pretty


-- | Load and transform a module.
cmdLoad :: Bundle -> Source -> String -> IO ()
cmdLoad bundle source str
 | Bundle fragment _ zero simpl _    <- bundle
 = do   errs    <- pipeText (nameOfSource source) (lineStartOfSource source) str
                $  PipeTextLoadCore  fragment
                [  PipeCoreSimplify  fragment zero simpl
                [  PipeCoreCheck     fragment
                [  PipeCoreOutput    SinkStdout ]]]

        mapM_ (putStrLn . renderIndent . ppr) errs


