
module DDCI.Core.Command.Sea
        (cmdSeaOut)
where
import DDCI.Core.Mode
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDCI.Core.Language
import DDC.Base.Pretty
import qualified Data.Set               as Set


-- | Parse, check, and fully evaluate an expression.
cmdSeaOut :: State -> Source -> String -> IO ()
cmdSeaOut state source str
 = do   errs    <- pipeText source str
                $  PipeTextLoadCore  fragmentSea
                [  PipeCoreSimplify  (stateSimplifier state)
                [  PipeCoreCheck     fragmentSea 
                [  PipeCoreAsSea
                [  PipeSeaPrint 
                        (Set.member SeaPrelude (stateModes state))
                        SinkStdout ]]]]

        mapM_ (putStrLn . renderIndent . ppr) errs
