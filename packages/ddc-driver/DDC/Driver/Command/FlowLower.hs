
module DDC.Driver.Command.FlowLower
        (cmdFlowLower)
where
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
-- import DDC.Data.Canned
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
-- import qualified DDC.Build.Language.Flow                as Flow
-- import qualified DDC.Core.Flow.Transform.Slurp          as Flow
-- import qualified DDC.Core.Flow.Transform.Schedule       as Flow
-- import qualified DDC.Core.Flow.Transform.Extract        as Flow
import qualified DDC.Base.Pretty                        as P


-- | Lower a flow program to loop code.
cmdFlowLower
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowLower config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  stageFlowLoad  config source
                [  stageFlowLower config source
                [  PipeCoreOutput SinkStdout ]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es



{-
                $  PipeTextLoadCore Flow.fragment
                [  PipeCoreStrip
                [  PipeCoreHacks 
                    (Canned $ \m 
                     -> do let processes   = Flow.slurpProcesses m
                           let procs       = map Flow.scheduleProcess processes
                           let m'          = Flow.extractModule  m procs
                           return m')
                [  PipeCoreOutput SinkStdout ]]]
-}
