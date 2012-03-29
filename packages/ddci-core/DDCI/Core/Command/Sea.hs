
module DDCI.Core.Command.Sea
        (cmdSeaOut)
where
import DDCI.Core.Pipeline.Module
import DDCI.Core.State
import DDC.Base.Pretty
import qualified Data.Set               as Set


-- | Parse, check, and fully evaluate an expression.
cmdSeaOut :: State -> Int -> String -> IO ()
cmdSeaOut state lineStart str
 = do   errs    <- pipeTextModule lineStart str
                $  PipeTextModuleLoadSea
                        [ PipeSeaModulePrint 
                                (Set.member SeaPrelude (stateModes state))
                                SinkStdout ]

        mapM_ (putStrLn . renderIndent . ppr) errs
