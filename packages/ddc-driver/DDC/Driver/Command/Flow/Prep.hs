
module DDC.Driver.Command.Flow.Prep
        (cmdFlowPrep)
where
import DDC.Driver.Stage
import DDC.Driver.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty        as P


-- | Prepare a Disciple Core Flow module for lowering.
cmdFlowPrep
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowPrep config source sourceText
 = do   
        errs    <- liftIO
                $  pipeText (nameOfSource source)
                            (lineStartOfSource source)
                            sourceText
                $  stageFlowLoad  config source
                [  stageFlowPrep  config source
                [  PipeCoreOutput SinkStdout ]]

        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

