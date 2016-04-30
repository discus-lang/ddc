
module DDC.Driver.Stage.Tetra
        ( stageSourceTetraLoad
        , stageTetraLoad
        , stageTetraToSalt)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Driver.Interface.Source
import DDC.Build.Interface.Store                (Store)
import DDC.Build.Pipeline
import DDC.Base.Pretty
import qualified DDC.Build.Language.Tetra       as BE
import qualified DDC.Build.Builder              as B
import qualified DDC.Core.Tetra                 as CE
import qualified DDC.Core.Salt                  as CS
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as C
import qualified DDC.Core.Transform.Namify      as C
import qualified DDC.Base.Parser                as BP


---------------------------------------------------------------------------------------------------
-- | Load and type check a Source Tetra module.
stageSourceTetraLoad
        :: Config -> Source
        -> Store
        -> [PipeCore (C.AnTEC BP.SourcePos CE.Name) CE.Name]
        -> PipeText CE.Name CE.Error

stageSourceTetraLoad config source store pipesTetra
 = PipeTextLoadSourceTetra
                    (dump config source "dump.tetra-load-tokens.txt")
                    (dump config source "dump.tetra-load-desugared.dct")
                    (dump config source "dump.tetra-load-core.dct")
                    (dump config source "dump.tetra-load-precheck.dct")
                    (dump config source "dump.tetra-load-trace.txt")
                    store
   ( PipeCoreOutput pprDefaultMode
                    (dump config source "dump.tetra-load.dct")
   : pipesTetra ) 


---------------------------------------------------------------------------------------------------
-- | Load and type check a Core Tetra module.
stageTetraLoad
        :: Config -> Source
        -> [PipeCore () CE.Name]
        -> PipeText CE.Name CE.Error

stageTetraLoad config source pipesTetra
 = PipeTextLoadCore BE.fragment 
        (if configInferTypes config then C.Synth else C.Recon)
                         (dump config source "dump.tetra-check.dct")
 [ PipeCoreReannotate (const ())
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.tetra-load.dct")
        : pipesTetra ) ]
 

---------------------------------------------------------------------------------------------------
-- | Convert a Core Tetra module to Core Salt.
--
--   This includes performing the Boxing transform.
---
--   The Tetra to Salt transform requires the program to be normalised,
--   and have type annotations.
stageTetraToSalt 
        :: Config -> Source
        -> [PipeCore () CS.Name] 
        -> PipeCore  () CE.Name

stageTetraToSalt config source pipesSalt
 = pipe_lambdas
 where
        pipe_lambdas
         = PipeCoreCheck        BE.fragment C.Recon SinkDiscard
           [ PipeCoreSimplify   BE.fragment (0 :: Int) C.lambdas 
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.tetra-lambdas.dct")
           , pipe_curry]]

        pipe_curry
         = PipeCoreCheck        BE.fragment C.Recon SinkDiscard
           [ PipeCoreAsTetra
           [ PipeTetraCurry
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.tetra-curry.dct")
           , pipe_prep ]]]

        pipe_prep
         = PipeCoreSimplify     BE.fragment 0 normalize
           [ pipe_boxing ]

        normalize
         = C.anormalize
                (C.makeNamifier CE.freshT)      
                (C.makeNamifier CE.freshX)

        pipe_boxing
         = PipeCoreAsTetra
           [ PipeTetraBoxing
             [ PipeCoreOutput   pprDefaultMode
                                (dump config source "dump.tetra-boxing-raw.dct")
             , PipeCoreSimplify BE.fragment 0 (normalize `mappend` C.flatten)
               [ PipeCoreOutput pprDefaultMode
                                (dump config source "dump.tetra-boxing-simp.dct")
               , pipe_toSalt]]]


        pipe_toSalt           
         = PipeCoreCheck        BE.fragment C.Recon SinkDiscard
           [ PipeCoreAsTetra
             [ PipeTetraToSalt  (B.buildSpec $ configBuilder config) 
                                (configRuntime config)
             ( PipeCoreOutput   pprDefaultMode
                                (dump config source "dump.salt.dcs")
             : pipesSalt)]]

