
module DDC.Driver.Command.Flow.Lower
        (cmdFlowLower)
where
import DDC.Driver.Stage                         as Driver
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow                  as Flow
import qualified DDC.Core.Transform.Suppress    as Suppress


-- | Lower a flow program to loop code.
cmdFlowLower
        :: Driver.Config        -- ^ Driver config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Suppress.Config      -- ^ Pretty printer suppression config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowLower
        configDriver configLower configSupp
        source sourceText
 = let  
        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  configDriver source 
         [  stageFlowPrep  configDriver source
         [  PipeCoreCheck  Flow.fragment C.Recon
         [  stageFlowLower configDriver configLower source [ pipeFinal ]]]]

        pipeFinal
         | configTaintAvoidTypeChecks configDriver
         = PipeCoreSuppress configSupp
         [ PipeCoreOutput SinkStdout ]

         | otherwise
         = PipeCoreCheck Flow.fragment C.Recon
         [ PipeCoreSuppress configSupp
         [ PipeCoreOutput SinkStdout ]]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

