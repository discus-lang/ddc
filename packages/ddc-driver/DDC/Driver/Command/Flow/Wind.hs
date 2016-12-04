
module DDC.Driver.Command.Flow.Wind
        (cmdFlowWind)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Data.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow


-- | Lower a flow program to loop code.
cmdFlowWind
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ExceptT String IO ()

cmdFlowWind config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config

        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  config source 
         [  PipeCoreCheck  "FlowWind" Flow.fragment C.Recon SinkDiscard
         [  stageFlowWind  config source [ pipeFinal ]]]

        pipeFinal
         | configTaintAvoidTypeChecks config
         = PipeCoreOutput pmode SinkStdout

         | otherwise
         = PipeCoreCheck  "FlowWind" Flow.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es
