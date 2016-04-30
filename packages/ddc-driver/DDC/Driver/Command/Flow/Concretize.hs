
module DDC.Driver.Command.Flow.Concretize
        (cmdFlowConcretize)
where
import DDC.Build.Pipeline
import DDC.Build.Language.Flow
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Data.Canned
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Core.Flow.Transform.Concretize     as Concretize
import qualified DDC.Core.Check                         as C
import qualified DDC.Base.Pretty                        as P


-- | Concretize rate variables to loop indices.
cmdFlowConcretize
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ExceptT String IO ()

cmdFlowConcretize config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config

        pipeConcretize
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $ PipeTextLoadCore fragment C.Recon SinkDiscard
         [ PipeCoreReannotate (const ())
         [ PipeCoreHacks 
                (Canned $ \m -> return 
                        $  Concretize.concretizeModule m)
         [ PipeCoreOutput pmode SinkStdout ]]]
   in do
        errs    <- liftIO pipeConcretize
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es



