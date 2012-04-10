
module DDCI.Core.Command.Sea
        (cmdSeaOut)
where
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDCI.Core.Language
import DDC.Core.Simplifier.Recipie      as Simpl
import qualified Data.Set               as Set
import qualified DDC.Base.Pretty        as P
import Data.Monoid


-- | Parse, check, and fully evaluate an expression.
---
--   The Core -> C conversion only accepts A-normalised programs,
--   so we normalize it along the way.
cmdSeaOut :: State -> Source -> String -> IO ()
cmdSeaOut state source str
 = (pipeText source str
        $  PipeTextLoadCore     fragmentSea
        [  PipeCoreSimplify     fragmentSea     
                                (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreCheck        fragmentSea 
        [  PipeCoreAsSea
        [  PipeSeaPrint 
                (Set.member SeaPrelude (stateModes state))
                SinkStdout ]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)
