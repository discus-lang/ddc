
module DDCI.Core.Command.Llvm
        (cmdLlvmOut)
where
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Base.Pretty


-- | Parse, check and convert a Sea module to LLVM.
cmdLlvmOut :: State -> Int -> String -> IO ()
cmdLlvmOut _state lineStart str
 = do   errs    <- pipeTextModule lineStart str
                $  PipeTextModuleLoadSea
                   [ PipeSeaModuleToLlvm
                        [ PipeLlvmModulePrint SinkStdout ]
                   ]

        mapM_ (putStrLn . renderIndent . ppr) errs

-- TODO: set platform
