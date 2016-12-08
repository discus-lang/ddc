
module DDC.Driver.Command.Machine.Prep
        (cmdMachinePrep)
where
import DDC.Driver.Stage
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Core.Check                 as C
import qualified DDC.Data.Pretty                as P
import qualified DDC.Build.Language.Machine     as Machine


-- | Prepare a Disciple Core Machine module for lowering.
cmdMachinePrep
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdMachinePrep config source sourceText
 = let  pmode   = prettyModeOfConfig $ configPretty config
        
        pipePrep
         = pipeText (nameOfSource source)
                     (lineStartOfSource source)
                     sourceText
         $ stageMachineLoad  config source 
         [ stageMachinePrep  config source
         [ PipeCoreCheck  "MachinePrep" Machine.fragment C.Recon SinkDiscard
         [ PipeCoreOutput pmode SinkStdout ]]]
   in do
        errs    <- liftIO pipePrep
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es


