
module DDCI.Core.Command.ToC
        (cmdToC)
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
cmdToC :: State -> Source -> String -> IO ()
cmdToC state source str
 = (pipeText source str
        $  PipeTextLoadCore     fragmentBrine
        [  PipeCoreSimplify     fragmentBrine   
                                (stateSimplifier state <> Simpl.anormalize)
        [  PipeCoreCheck        fragmentBrine
        [  PipeCoreAsBrine
        [  PipeBrinePrint 
                (Set.member BrinePrelude (stateModes state))
                SinkStdout ]]]])
 >>= mapM_ (putStrLn . P.renderIndent . P.ppr)
