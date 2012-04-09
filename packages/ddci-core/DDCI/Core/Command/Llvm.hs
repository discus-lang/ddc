
module DDCI.Core.Command.Llvm
        (cmdLlvmOut)
where
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Base.Pretty


-- | Parse, check and convert a Sea module to LLVM.
cmdLlvmOut :: State -> Source -> String -> IO ()
cmdLlvmOut state source str
 = do   errs    <- pipeText source str
                $  PipeTextLoadCore  fragmentSea
                [  PipeCoreSimplify  fragmentSea (stateSimplifier state)
                [  PipeCoreCheck     fragmentSea
                [  PipeCoreAsSea
                [  PipeSeaToLlvm 
                [  PipeLlvmPrint SinkStdout ]]]]]

        mapM_ (putStrLn . renderIndent . ppr) errs
