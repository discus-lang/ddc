
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
import DDC.Data.Pretty
import qualified DDC.Build.Language.Tetra       as BE
import qualified DDC.Build.Builder              as B
import qualified DDC.Core.Tetra                 as CE
import qualified DDC.Core.Salt                  as CS
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as C
import qualified DDC.Core.Transform.Namify      as C
import qualified DDC.Control.Parser                as BP


---------------------------------------------------------------------------------------------------
-- | Load and type check a Source Tetra module.
stageSourceTetraLoad
        :: Config -> Source
        -> Store
        -> [PipeCore (C.AnTEC BP.SourcePos CE.Name) CE.Name]
        -> PipeText CE.Name CE.Error

stageSourceTetraLoad config source store pipesTetra
 = PipeTextLoadSourceTetra
                    (dump config source "dump.0-source-01-tokens.txt")
                    (dump config source "dump.0-source-02-parsed.dst")
                    (dump config source "dump.0-source-03-fresh.dst")
                    (dump config source "dump.0-source-04-defix.dst")
                    (dump config source "dump.0-source-05-expand.dst")
                    (dump config source "dump.0-source-06-guards.dst")
                    (dump config source "dump.0-source-07-matches.dst")
                    (dump config source "dump.0-source-08-prep.dst")
                    (dump config source "dump.0-source-09-core.dct")
                    (dump config source "dump.0-source-10-precheck.dct")
                    (dump config source "dump.0-source-11-trace.txt")
                    store
   [ PipeCoreOutput pprDefaultMode
                    (dump config source "dump.1-tetra-00-checked.dct")
   , PipeCoreResolve "SourceTetraLoad" BE.fragment
      (PipeCoreOutput   pprDefaultMode
                        (dump config source "dump.1-tetra-01-resolve.dct")
      : pipesTetra)]
        


---------------------------------------------------------------------------------------------------
-- | Load and type check a Core Tetra module.
stageTetraLoad
        :: Config -> Source
        -> [PipeCore () CE.Name]
        -> PipeText CE.Name CE.Error

stageTetraLoad config source pipesTetra
 = PipeTextLoadCore BE.fragment 
        (if configInferTypes config then C.Synth [] else C.Recon)
                         (dump config source "dump.1-tetra-00-check.dct")

 [ PipeCoreResolve "TetraLoad" BE.fragment
   [ PipeCoreOutput     pprDefaultMode
                        (dump config source "dump.1-tetra-01-resolve.dct")
   , PipeCoreReannotate (const ()) pipesTetra ]]


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
 =  pipe_expliciate
 where
        pipe_expliciate
         = PipeCoreSimplify     BE.fragment (0 :: Int) C.expliciate
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.1-tetra-02-expliciate.dct")
           , pipe_lambdas]

        pipe_lambdas
         = PipeCoreCheck        "TetraToSalt/lambdas" BE.fragment C.Recon SinkDiscard
           [ PipeCoreSimplify   BE.fragment (0 :: Int) C.lambdas 
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.1-tetra-03-lambdas.dct") 
           , pipe_curry]]

        pipe_curry
         = PipeCoreCheck        "TetraToSalt/curry" BE.fragment C.Recon SinkDiscard
           [ PipeCoreAsTetra
           [ PipeTetraCurry     (dump config source "dump.1-tetra-04-unshare.dct")
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.1-tetra-05-curry.dct")
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
                                (dump config source "dump.1-tetra-06-boxing-raw.dct")
             , PipeCoreSimplify BE.fragment 0 (normalize `mappend` C.flatten)
               [ PipeCoreOutput pprDefaultMode
                                (dump config source "dump.1-tetra-07-boxing-simp.dct")
               , pipe_toSalt]]]


        pipe_toSalt           
         = PipeCoreCheck        "TetraToSalt/toSalt" BE.fragment C.Recon SinkDiscard
           [ PipeCoreAsTetra
             [ PipeTetraToSalt  (B.buildSpec $ configBuilder config) 
                                (configRuntime config)
             ( PipeCoreOutput   pprDefaultMode
                                (dump config source "dump.2-salt-00-convert.dcs")
             : pipesSalt)]]
