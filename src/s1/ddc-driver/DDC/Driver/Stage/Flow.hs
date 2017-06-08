
module DDC.Driver.Stage.Flow
        ( stageFlowLoad
        , stageFlowPrep
        , stageFlowRate
        , stageFlowLower
        , stageFlowWind
        , stageFlowToTetra)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Pipeline
import DDC.Data.Pretty
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Flow                  as Flow
import qualified DDC.Build.Language.Flow        as Flow
import qualified DDC.Core.Salt                  as Salt


---------------------------------------------------------------------------------------------------
-- | Type check Core Flow.
stageFlowLoad
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        -> PipeText Flow.Name Flow.Error

stageFlowLoad config source pipesFlow
 = PipeTextLoadCore Flow.fragment 
        (if configInferTypes config then C.Synth [] else C.Recon)
                         (dump config source "dump.flow-check.txt")
   [ PipeCoreReannotate  (const ()) 
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.flow-load.dcf")
        : pipesFlow ) ]


---------------------------------------------------------------------------------------------------
-- | Prepare a Core Flow module for lowering.
stageFlowPrep
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore () Flow.Name

stageFlowPrep config source pipesFlow
 = PipeCoreReannotate   (const ())
 [ PipeCoreAsFlow
   [ PipeFlowPrep
     ( PipeCoreOutput   pprDefaultMode
                        (dump config source "dump.flow-prep.dcf")
     : pipesFlow)]]


---------------------------------------------------------------------------------------------------
-- | Perform rate inference to transform vector operations to series
stageFlowRate
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore () Flow.Name

stageFlowRate config source pipesFlow
 = PipeCoreReannotate   (const ())
 [ PipeCoreAsFlow
   [ PipeFlowRate
     ( PipeCoreOutput   pprDefaultMode
                        (dump config source "dump.flow-rate.dcf")
     : pipesFlow)]]
 

---------------------------------------------------------------------------------------------------
-- | Lower a Core Flow module.
--   Is needs to already be prepped,
--   and have full type annotations.
stageFlowLower
        :: Config -> Flow.Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore (C.AnTEC () Flow.Name) Flow.Name

stageFlowLower config lowerConfig source pipesFlow 
 = PipeCoreAsFlow
     [ PipeFlowLower lowerConfig
       ( PipeCoreOutput pprDefaultMode
                        (dump config source "dump.flow-lower.dcf")
       : pipesFlow ) ]


---------------------------------------------------------------------------------------------------
-- | Wind loop primops into tail recursive loops in a Core Flow module.
stageFlowWind
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore (C.AnTEC () Flow.Name) Flow.Name

stageFlowWind config source pipesFlow
 = PipeCoreAsFlow
     [ PipeFlowWind
       ( PipeCoreOutput pprDefaultMode
                        (dump config source "dump.flow-wind.dcf")
       : pipesFlow ) ]


---------------------------------------------------------------------------------------------------
-- | Wind loop primops into tail recursive loops in a Core Flow module.
stageFlowToTetra
        :: Config -> Source
        -> [PipeCore () Salt.Name]
        ->  PipeCore (C.AnTEC () Flow.Name) Flow.Name

stageFlowToTetra config source pipesTetra
 = PipeCoreAsFlow
     [ PipeFlowToTetra
       ( PipeCoreOutput pprDefaultMode
                        (dump config source "dump.flow-out-tetra.dct")
       : pipesTetra ) ]

