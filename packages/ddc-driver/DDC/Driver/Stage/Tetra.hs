
module DDC.Driver.Stage.Tetra
        ( stageSourceTetraLoad)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Base.Pretty

import qualified DDC.Core.Tetra                 as CE
import qualified DDC.Core.Annot.AnTEC           as C

import qualified DDC.Base.Parser                as BP


-- | Load and type check a source tetra file.
stageSourceTetraLoad
        :: Config -> Source
        -> [PipeCore (C.AnTEC BP.SourcePos CE.Name) CE.Name]
        -> PipeText CE.Name CE.Error

stageSourceTetraLoad config source pipesTetra
 = PipeTextLoadSourceTetra
   ( PipeCoreOutput pprDefaultMode
                    (dump config source "dump.tetra-load.dct")
   : pipesTetra ) 
