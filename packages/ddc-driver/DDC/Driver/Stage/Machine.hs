
module DDC.Driver.Stage.Machine
        ( stageMachineLoad
        , stageMachinePrep
        , stageMachineOutputSlurp)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Data.Pretty
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Machine               as Machine
import qualified DDC.Build.Language.Machine     as Machine


---------------------------------------------------------------------------------------------------
-- | Type check Core Machine.
stageMachineLoad
        :: Config -> Source
        -> [PipeCore () Machine.Name]
        -> PipeText Machine.Name Machine.Error

stageMachineLoad config source pipesMachine
 = PipeTextLoadCore Machine.fragment 
        (if configInferTypes config then C.Synth [] else C.Recon)
                         (dump config source "dump.machine-check.txt")
   [ PipeCoreReannotate  (const ()) 
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.machine-load.dcm")
        : pipesMachine ) ]


---------------------------------------------------------------------------------------------------
-- | Prepare a Core Machine module for lowering.
stageMachinePrep
        :: Config -> Source
        -> [PipeCore () Machine.Name]
        ->  PipeCore () Machine.Name

stageMachinePrep config source pipesMachine
 = PipeCoreReannotate   (const ())
 [ PipeCoreAsMachine
   [ PipeMachinePrep
     ( PipeCoreOutput   pprDefaultMode
                        (dump config source "dump.flow-prep.dcf")
     : pipesMachine)]]

---------------------------------------------------------------------------------------------------
-- | Prepare a Core Machine module for lowering.
stageMachineOutputSlurp
        :: Sink
        -> PipeCore () Machine.Name

stageMachineOutputSlurp sink
 = PipeCoreAsMachine
   [ PipeMachineOutputSlurp sink]

