
module DDCI.Core.Command.Load
        ( cmdLoad
        , loadModule)
where
import DDCI.Core.Pipeline.Module
import DDCI.Core.Mode
import DDCI.Core.State
import DDC.Core.Load
import DDC.Core.Pretty


-- | Parse, check, and single step evaluate an expression.
--   TODO: check capabilities in module.
cmdLoad :: State -> Source -> String -> IO ()
cmdLoad state source str
 = do   errs    <- pipeTextModule source str
                $  PipeTextModuleLoadCore (stateLanguage state)
                        [ PipeCoreModuleOutput 
                                SinkStdout ]


        mapM_ (putStrLn . renderIndent . ppr) errs
