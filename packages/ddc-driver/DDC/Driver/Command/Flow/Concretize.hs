
module DDC.Driver.Command.Flow.Concretize
        (cmdFlowConcretize)
where
import DDC.Build.Pipeline
import DDC.Build.Language.Flow
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Data.Canned
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Flow.Transform.Concretize     as Concretize
import qualified DDC.Base.Pretty                        as P

cmdFlowConcretize
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowConcretize _config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  PipeTextLoadCore fragment
                [  PipeCoreReannotate (const ())
                [  PipeCoreHacks 
                   (Canned $ \m -> return 
                           $  Concretize.concretizeModule m)
                [  PipeCoreOutput SinkStdout ]]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es



