
module DDC.Driver.Command.Flow.Rate
        (cmdFlowRate)
where
import DDC.Driver.Stage                         as Driver
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow


-- | Perform rate inference to transform vector operations to series
cmdFlowRate
        :: Bool                 -- ^ Whether to use bidirectional type inference.
        -> Sink                 -- ^ Where to send type checker trace.
        -> Driver.Config        -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowRate 
        useBidirChecking
        sinkCheckerTrace
        configDriver 
        source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  stageFlowLoad  configDriver source 
                                  useBidirChecking sinkCheckerTrace
                [  stageFlowRate  configDriver source 
                [  PipeCoreCheck  Flow.fragment C.Recon
                [  PipeCoreOutput SinkStdout ]]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

