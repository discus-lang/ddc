
module DDC.Driver.Command.Flow.Lower
        (cmdFlowLower)
where
import DDC.Driver.Stage                         as Driver
import DDC.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import qualified DDC.Base.Pretty                as P
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Flow                  as Flow
import qualified DDC.Core.Transform.Suppress    as Suppress


-- | Lower a flow program to loop code.
cmdFlowLower
        :: Suppress.Config
        -> Driver.Config
        -> Flow.Config  -- ^ Config for the lowering transform.
        -> Source       -- ^ Source of the code.
        -> String       -- ^ Program module text.
        -> ErrorT String IO ()

cmdFlowLower supp config lowerConfig source sourceText
 = let  
        pipeLower
         = pipeText (nameOfSource source)
                    (lineStartOfSource source)
                    sourceText
         $  stageFlowLoad  config source
         [  stageFlowPrep  config source
         [  PipeCoreCheck  Flow.fragment
         [  stageFlowLower config lowerConfig source [ pipeFinal ]]]]

        pipeFinal
         | configTaintAvoidTypeChecks config
         = PipeCoreSuppress supp
         [ PipeCoreOutput SinkStdout ]

         | otherwise
         = PipeCoreCheck Flow.fragment
         [ PipeCoreSuppress supp
         [ PipeCoreOutput SinkStdout ]]

   in do        
        errs    <- liftIO pipeLower
        case errs of
         []     -> return ()
         es     -> throwError $ P.renderIndent $ P.vcat $ map P.ppr es
