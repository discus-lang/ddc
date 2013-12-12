
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
        :: Bool                 -- ^ Whether to do bidir type inference.
        -> Suppress.Config      -- ^ Pretty printer suppression config.
        -> Driver.Config        -- ^ Platform config.
        -> Flow.Config          -- ^ Config for the lowering transform.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowLower bidir supp config lowerConfig source sourceText
 = let  
        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  config source
         [  stageFlowPrep  config source
         [  PipeCoreCheck  Flow.fragment 
                           (if bidir then C.Synth else C.Recon)
         [  stageFlowLower config lowerConfig source [ pipeFinal ]]]]

        pipeFinal
         | configTaintAvoidTypeChecks config
         = PipeCoreSuppress supp
         [ PipeCoreOutput SinkStdout ]

         | otherwise
         = PipeCoreCheck Flow.fragment C.Recon
         [ PipeCoreSuppress supp
         [ PipeCoreOutput SinkStdout ]]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es

