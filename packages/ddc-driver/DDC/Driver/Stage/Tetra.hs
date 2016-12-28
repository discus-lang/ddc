
module DDC.Driver.Stage.Tetra
        ( sourceLoadText

        , stageTetraLoad
        , tetraLoadText

        , stageTetraToSalt
        , tetraToSalt)
where
import DDC.Data.Pretty
import Control.Monad.Trans.Except

import qualified DDC.Data.SourcePos             as SP

import DDC.Driver.Dump
import DDC.Driver.Config
import qualified DDC.Driver.Interface.Source    as D

import DDC.Build.Pipeline
import qualified DDC.Build.Interface.Store      as B
import qualified DDC.Build.Pipeline.Error       as B
import qualified DDC.Build.Builder              as B
import qualified DDC.Build.Stage.Core           as B
import qualified DDC.Build.Language.Tetra       as BE
import qualified DDC.Build.Stage.Source.Tetra   as BST
import qualified DDC.Build.Stage.Core.Tetra     as BCT

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as C
import qualified DDC.Core.Transform.Namify      as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Tetra                 as E
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Transform.Reannotate  as CReannotate



---------------------------------------------------------------------------------------------------
-- | Load and type-check a source tetra module.
sourceLoadText
        :: Config               -- ^ Driver config.
        -> B.Store              -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO 
                   (C.Module (C.AnTEC SP.SourcePos E.Name) E.Name)

sourceLoadText config store source str
 = BST.sourceLoadText 
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
        store
 $ BST.ConfigLoadSourceTetra 
        { BST.configSinkTokens          = dump config source "dump.0-source-01-tokens.txt"
        , BST.configSinkParsed          = dump config source "dump.0-source-02-parsed.dst"
        , BST.configSinkFresh           = dump config source "dump.0-source-03-fresh.dst"
        , BST.configSinkDefix           = dump config source "dump.0-source-04-defix.dst"
        , BST.configSinkExpand          = dump config source "dump.0-source-05-expand.dst"
        , BST.configSinkGuards          = dump config source "dump.0-source-06-guards.dst"
        , BST.configSinkMatches         = dump config source "dump.0-source-07-matches.dst"
        , BST.configSinkPrep            = dump config source "dump.0-source-08-prep.dst"
        , BST.configSinkCore            = dump config source "dump.0-source-09-core.dct"
        , BST.configSinkPreCheck        = dump config source "dump.0-source-10-precheck.dct"
        , BST.configSinkCheckerTrace    = dump config source "dump.0-source-11-trace.txt"
        , BST.configSinkChecked         = dump config source "dump.0-source-12-checked.dct"
        , BST.configSinkElaborated      = dump config source "dump.0-source-13-elaborated.dct"
        }


---------------------------------------------------------------------------------------------------
-- | Load and type check a Core Tetra module.
stageTetraLoad
        :: Config 
        -> D.Source 
        -> B.Store
        -> [PipeCore () E.Name]
        -> PipeText E.Name E.Error

stageTetraLoad config source store pipesTetra
 = PipeTextLoadCore BE.fragment 
        (if configInferTypes config then C.Synth [] else C.Recon)
                         (dump config source "dump.1-tetra-00-check.dct")

 [ PipeCoreResolve "TetraLoad" BE.fragment (B.importValuesOfStore store)
   [ PipeCoreOutput     pprDefaultMode
                        (dump config source "dump.1-tetra-01-resolve.dct")
   , PipeCoreReannotate (const ()) pipesTetra ]]


-- | Load and type-check a core tetra module.
tetraLoadText 
        :: Config               -- ^ Driver config.
        -> B.Store              -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos E.Name) E.Name)

tetraLoadText config _store source str
 = B.coreLoad
        "TetraLoad"
        BE.fragment
        (if configInferTypes config then C.Synth [] else C.Recon)
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        (dump config source "dump.1-tetra-00-check.dct")
        str


---------------------------------------------------------------------------------------------------
-- | Convert a Core Tetra module to Core Salt.
--
--   This includes performing the Boxing transform.
---
--   The Tetra to Salt transform requires the program to be normalised,
--   and have type annotations.
stageTetraToSalt 
        :: Config 
        -> D.Source
        -> [PipeCore () A.Name] 
        -> PipeCore  () E.Name

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
                (C.makeNamifier E.freshT)      
                (C.makeNamifier E.freshX)

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


-- | Convert Core Tetra module to Core Salt.
tetraToSalt
        :: Config
        -> D.Source
        -> C.Module a E.Name
        -> ExceptT [B.Error] IO (C.Module () A.Name)

tetraToSalt config source mm
 = BCT.tetraToSalt
        (B.buildSpec $ configBuilder config)
        (configRuntime config)
        (CReannotate.reannotate (const ()) mm)
 $ BCT.ConfigTetraToSalt
        { BCT.configSinkExplicit        = dump config source "dump-1-tetra-02-explicit.dct"
        , BCT.configSinkLambdas         = dump config source "dump-1-tetra-03-lambdas.dct"
        , BCT.configSinkUnshare         = dump config source "dump-1-tetra-04-unshare.dct"
        , BCT.configSinkCurry           = dump config source "dump-1-tetra-05-curry.dct"
        , BCT.configSinkBoxing          = dump config source "dump-1-tetra-06-boxing.dct"
        , BCT.configSinkPrep            = dump config source "dump-1-tetra-07-prep.dct"
        , BCT.configSinkChecked         = dump config source "dump-1-tetra-08-checked.dct"
        , BCT.configSinkSalt            = dump config source "dump-2-salt-00-convert.dcs"
        }

