
module DDCI.Core.Command.Llvm
        (cmdLlvmOut)
where
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Base.Pretty


-- | Parse, check and convert a Sea module to LLVM.
cmdLlvmOut :: State -> Source -> String -> IO ()
cmdLlvmOut _state source str
 = do   errs    <- pipeTextModule source str
                $  PipeTextModuleLoadSea
                   [ PipeSeaModuleToLlvm
                        [ PipeLlvmModulePrint SinkStdout ]
                   ]

        mapM_ (putStrLn . renderIndent . ppr) errs
