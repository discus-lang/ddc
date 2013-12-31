
module DDC.Driver.Command.Flow.Prep
        (cmdFlowPrep)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Core.Check                 as C
import qualified DDC.Base.Pretty                as P
import qualified DDC.Build.Language.Flow        as Flow


-- | Prepare a Disciple Core Flow module for lowering.
cmdFlowPrep
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowPrep config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipePrep
         = pipeText (nameOfSource source)
                     (lineStartOfSource source)
                     sourceText
         $ stageFlowLoad  config source 
         [ stageFlowPrep  config source
         [ PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]
   in do
        errs    <- liftIO pipePrep
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

