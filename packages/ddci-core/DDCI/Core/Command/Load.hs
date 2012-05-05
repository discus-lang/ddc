
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Pipeline
import DDC.Core.Load
import DDC.Core.Pretty


-- | Load and transform a module.
cmdLoad :: State -> Source -> String -> IO ()
cmdLoad state source str
 | Language fragment    <- stateLanguage state
 = do   errs    <- pipeText (nameOfSource source) (lineStartOfSource source) str
                $  PipeTextLoadCore  fragment
                [  PipeCoreOutput    SinkStdout]
--                [  PipeCoreSimplify  fragment (stateSimplifier state)
--                [  PipeCoreCheck     fragment
--                [  PipeCoreOutput    SinkStdout ]]]

        mapM_ (putStrLn . renderIndent . ppr) errs
