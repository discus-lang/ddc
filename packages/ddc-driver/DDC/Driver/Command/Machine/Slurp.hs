
module DDC.Driver.Command.Machine.Slurp
        ( cmdMachineOutputSlurp
        , cmdMachineOutputFused)
where
import DDC.Driver.Stage
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import qualified DDC.Data.Pretty                as P


-- | Prepare a Disciple Core Machine module for lowering.
cmdMachineOutputSlurp
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdMachineOutputSlurp config source sourceText
 = let  pipePrep
         = pipeText (nameOfSource source)
                     (lineStartOfSource source)
                     sourceText
         $ stageMachineLoad  config source 
         [ stageMachinePrep  config source
         [ stageMachineOutputSlurp SinkStdout ]]
   in do
        errs    <- liftIO pipePrep
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es


-- | Prepare a Disciple Core Machine module for lowering.
cmdMachineOutputFused
        :: Config               -- ^ Driver config.
        -> Source               -- ^ Source of the code.
        -> String               -- ^ Program module text.
        -> ExceptT String IO ()

cmdMachineOutputFused config source sourceText
 = let  pipePrep
         = pipeText (nameOfSource source)
                     (lineStartOfSource source)
                     sourceText
         $ stageMachineLoad  config source 
         [ stageMachinePrep  config source
         [ stageMachineOutputFused SinkStdout ]]
   in do
        errs    <- liftIO pipePrep
        case errs of
         []     -> return ()
         es     -> throwE $ P.renderIndent $ P.vcat $ map P.ppr es


