
module DDC.Driver.Stage.Tetra
        ( stageSourceTetraLoad
        , stageTetraToSalt)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Pipeline
import DDC.Base.Pretty

import qualified DDC.Build.Language.Tetra       as BE
import qualified DDC.Build.Builder              as B

import qualified DDC.Core.Tetra.Profile         as CE
import qualified DDC.Core.Tetra                 as CE

import qualified DDC.Core.Salt                  as CS

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as C
import qualified DDC.Core.Transform.Namify      as C

import qualified DDC.Base.Parser                as BP


-------------------------------------------------------------------------------
-- | Load and type check a source tetra file.
stageSourceTetraLoad
        :: Config -> Source
        -> [PipeCore (C.AnTEC BP.SourcePos CE.Name) CE.Name]
        -> PipeText CE.Name CE.Error

stageSourceTetraLoad config source pipesTetra
 = PipeTextLoadSourceTetra
                    (dump config source "dump-tetra-load-raw.dct")
                    (dump config source "dump-tetra-load-trace.txt")
   ( PipeCoreOutput pprDefaultMode
                    (dump config source "dump.tetra-load.dct")
   : pipesTetra ) 

 
-------------------------------------------------------------------------------
-- | Convert Core Tetra to Core Salt.
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
 = pipe_norm
 where
        pipe_norm
         = PipeCoreSimplify     BE.fragment 0 normalize
           [ PipeCoreCheck      BE.fragment C.Recon SinkDiscard
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.tetra-normalized.dct")
           , pipe_boxing ]]

        normalize
         = C.anormalize
                (C.makeNamifier CE.freshT)      
                (C.makeNamifier CE.freshX)

        pipe_boxing
         = PipeCoreAsTetra      
           [ PipeTetraBoxing
           [ PipeCoreSimplify   BE.fragment 0 normalize
           [ PipeCoreCheck      BE.fragment C.Recon SinkDiscard
             [ PipeCoreOutput   pprDefaultMode
                                (dump config source "dump.tetra-boxing.dct")
             , pipe_toSalt]]]]

        pipe_toSalt
         = PipeCoreAsTetra
           [ PipeTetraToSalt    (B.buildSpec $ configBuilder config) 
                                (configRuntime config)
           ( PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.salt.dcs")
           : pipesSalt)]
           
