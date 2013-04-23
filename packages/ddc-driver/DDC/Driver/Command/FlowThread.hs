
module DDC.Driver.Command.FlowThread
        (cmdFlowThread)
where
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import DDC.Data.Canned
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Transform.Thread      as Thread
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow.Env              as Flow
import qualified DDC.Core.Flow.Compounds        as Flow
import qualified DDC.Base.Pretty                as P


-- | Thread a state token through the given flow program.
--     This can't be generic in the language fragment because we
--     need to provide a specific type to use for the world token,
--     and new types for the effectful combinators.
cmdFlowThread
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowThread _config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  PipeTextLoadCore Flow.fragment
                [  PipeCoreStrip
                [  PipeCoreHacks (Canned $ \m -> return $ Thread.thread threadConfig m)
                [  PipeCoreOutput SinkStdout ]]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es


threadConfig
        = Thread.Config
        { Thread.configDataDefs  = Flow.primDataDefs
        , Thread.configTokenType = Flow.tWorld
        , Thread.configThreadMe  = const Nothing }
