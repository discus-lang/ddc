
module DDC.Driver.Command.Flow.Thread
        (cmdFlowThread)
where
import DDC.Driver.Interface.Source
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Build.Pipeline
import DDC.Build.Language.Flow
import DDC.Core.Fragment
import DDC.Data.Canned
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Core.Transform.Thread      as Thread
import qualified DDC.Core.Flow.Transform.Thread as Flow
import qualified DDC.Core.Check                 as C
import qualified DDC.Base.Pretty                as P


-- | Thread a state token through the given flow program.
--     This can't be generic in the language fragment because we
--     need to provide a specific type to use for the world token,
--     and new types for the effectful combinators.
cmdFlowThread
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdFlowThread config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipeThread
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  PipeTextLoadCore fragment C.Recon SinkDiscard
         [  PipeCoreReannotate (const ())
         [  PipeCoreCheck    fragment C.Recon SinkDiscard
         [  PipeCoreHacks 
            (Canned $ \m -> return 
                    $  Thread.thread Flow.threadConfig 
                        (profilePrimKinds (fragmentProfile fragment))
                        (profilePrimTypes (fragmentProfile fragment)) m)
         [  PipeCoreOutput pmode SinkStdout ]]]]

   in do
        errs    <- liftIO pipeThread
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es

