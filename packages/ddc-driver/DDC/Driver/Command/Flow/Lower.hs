
module DDC.Driver.Command.Flow.Lower
        (cmdFlowLower)
where
import DDC.Driver.Stage                         as Driver
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow                  as Flow


-- | Lower a flow program to loop code.
cmdFlowLower
        :: Driver.Config        -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdFlowLower
        configDriver configLower
        source sourceText
 = let  
        pmode   = prettyModeOfConfig $ configPretty configDriver

        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  configDriver source 
         [  stageFlowPrep  configDriver source
         [  PipeCoreCheck  Flow.fragment C.Recon SinkDiscard
         [  stageFlowLower configDriver configLower source
         -- [ PipeCoreOutput pmode SinkStdout , pipeFinal ]]]]
         [ pipeFinal ]]]]

        pipeFinal
         | configTaintAvoidTypeChecks configDriver
         = PipeCoreOutput   pmode SinkStdout 

         | otherwise
         = PipeCoreCheck    Flow.fragment C.Recon SinkDiscard
         [ PipeCoreOutput   pmode SinkStdout ]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es

