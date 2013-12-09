
module DDC.Driver.Command.Flow.Wind
        (cmdFlowWind)
where
import DDC.Driver.Stage
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Core.Check                 as C
import qualified DDC.Build.Language.Flow        as Flow


-- | Lower a flow program to loop code.
cmdFlowWind
        :: Config
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowWind config source sourceText
 = let  
        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  config source
         [  PipeCoreCheck  Flow.fragment C.Recon
         [  stageFlowWind config source [ pipeFinal ]]]

        pipeFinal
         | configTaintAvoidTypeChecks config
         = PipeCoreOutput SinkStdout

         | otherwise
         = PipeCoreCheck Flow.fragment C.Recon
         [ PipeCoreOutput SinkStdout ]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es
