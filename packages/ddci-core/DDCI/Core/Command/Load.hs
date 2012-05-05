
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Interface.Suppress
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Build.Pipeline
import DDC.Core.Load
import DDC.Core.Pretty
import DDC.Data.Canned


-- | Load and transform a module.
cmdLoad :: State -> Source -> String -> IO ()
cmdLoad state source str
 | Language fragment    <- stateLanguage state
 = do   errs    <- pipeText (nameOfSource source) (lineStartOfSource source) str
                $  PipeTextLoadCore  fragment
                [  PipeCoreSimplify  fragment (stateSimplifier state)
                [  PipeCoreCheck     fragment
                [  PipeCoreHacks     (Canned (suppressModule state))
                [  PipeCoreOutput    SinkStdout ]]]]

        mapM_ (putStrLn . renderIndent . ppr) errs


