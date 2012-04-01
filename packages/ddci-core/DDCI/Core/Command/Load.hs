
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Pipeline.Module
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Core.Load
import DDC.Core.Pretty


-- | Load and transform a module.
cmdLoad :: State -> Source -> String -> IO ()
cmdLoad state source str
 = do   errs    <- pipeTextModule source str
                $  PipeTextModuleLoadCore  (stateLanguage  state)
                [  PipeCoreModuleTransform (stateTransform state)
                [  PipeCoreModuleOutput    SinkStdout ]]

        mapM_ (putStrLn . renderIndent . ppr) errs
