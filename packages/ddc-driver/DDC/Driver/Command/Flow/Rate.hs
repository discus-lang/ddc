
module DDC.Driver.Command.Flow.Rate
        ( cmdFlowRate
        , cmdFlowRateLower )
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow                  as Flow


-- | Perform rate inference to transform vector operations to series
cmdFlowRate
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowRate config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipeRate
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $ stageFlowLoad  config source 
         [ stageFlowRate  config source 
         [ PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]
   
   in do
        errs    <- liftIO pipeRate
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es


-- | Perform rate inference, then lower
cmdFlowRateLower
        :: Config               -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowRateLower config configLower source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipeRate
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $ stageFlowLoad  config source 
         [ stageFlowRate  config source 
         [ stageFlowPrep  config source
         [ PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
         [ stageFlowLower config configLower source
         [ PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]]]]
   
   in do
        errs    <- liftIO pipeRate
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

