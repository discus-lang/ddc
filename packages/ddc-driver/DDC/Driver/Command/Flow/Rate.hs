
module DDC.Driver.Command.Flow.Rate
        (cmdFlowRate)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow


-- | Perform rate inference to transform vector operations to series
cmdFlowRate
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowRate configDriver source sourceText
 = let  pipeRate
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $ stageFlowLoad  configDriver source 
         [ stageFlowRate  configDriver source 
         [ PipeCoreCheck  Flow.fragment C.Recon
         [ PipeCoreOutput SinkStdout ]]]
   
   in do
        errs    <- liftIO pipeRate
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

