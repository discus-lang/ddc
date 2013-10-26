
module DDC.Driver.Command.Flow.Rate
        (cmdFlowRate)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Build.Language.Flow        as Flow


-- | Perform rate inference to transform vector operations to series
cmdFlowRate
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowRate config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  stageFlowLoad  config source
                [  stageFlowRate  config source
                [  PipeCoreCheck Flow.fragment
                [  PipeCoreOutput SinkStdout ]]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

