
module DDCI.Core.Command.Llvm
        (cmdLlvmOut)
where
import DDCI.Core.Language
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified DDC.Base.Pretty        as P
import Data.Monoid


-- | Parse, check and convert a Sea module to LLVM.
---
--   The Core -> C conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdLlvmOut :: State -> Source -> String -> IO ()
cmdLlvmOut state source str
 = (pipeText source str
        $  PipeTextLoadCore     fragmentSea
        [  PipeCoreSimplify     fragmentSea 
                                (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreCheck        fragmentSea
        [  PipeCoreAsSea
        [  PipeSeaToLlvm 
        [  PipeLlvmPrint SinkStdout ]]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)
