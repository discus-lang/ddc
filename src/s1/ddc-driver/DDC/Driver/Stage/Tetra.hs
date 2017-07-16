
-- | Plumb driver configuration into the Tetra specific build stages.
module DDC.Driver.Stage.Tetra
        ( sourceLoadText
        , tetraLoadText
        , tetraToSalt
        , tetraToShimmer)
where
import Control.Monad.Trans.Except

import qualified DDC.Driver.Dump                as D
import qualified DDC.Driver.Config              as D
import qualified DDC.Driver.Interface.Source    as D

import qualified DDC.Build.Interface.Store      as B
import qualified DDC.Build.Pipeline.Error       as B
import qualified DDC.Build.Builder              as B
import qualified DDC.Build.Stage.Core           as B
import qualified DDC.Build.Language.Tetra       as BE
import qualified DDC.Build.Stage.Source.Tetra   as BST
import qualified DDC.Build.Stage.Core.Tetra     as BCT

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Tetra                 as CE
import qualified DDC.Core.Salt                  as CA
import qualified DDC.Core.Transform.Reannotate  as CReannotate

import qualified DDC.Core.SMR                   as H
import qualified DDC.Data.SourcePos             as SP
import DDC.Data.Pretty

---------------------------------------------------------------------------------------------------
-- | Load and type-check a source tetra module.
sourceLoadText
        :: D.Config             -- ^ Driver config.
        -> B.Store              -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos CE.Name) CE.Name)

sourceLoadText config store source str
 = BST.sourceLoad
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
        store
 $ BST.ConfigLoadSourceTetra
        { BST.configSinkTokens          = D.dump config source "dump.0-source-01-tokens.txt"
        , BST.configSinkParsed          = D.dump config source "dump.0-source-02-parsed.dst"
        , BST.configSinkFresh           = D.dump config source "dump.0-source-03-fresh.dst"
        , BST.configSinkDefix           = D.dump config source "dump.0-source-04-defix.dst"
        , BST.configSinkExpand          = D.dump config source "dump.0-source-05-expand.dst"
        , BST.configSinkGuards          = D.dump config source "dump.0-source-06-guards.dst"
        , BST.configSinkMatches         = D.dump config source "dump.0-source-07-matches.dst"
        , BST.configSinkPrep            = D.dump config source "dump.0-source-08-prep.dst"
        , BST.configSinkCore            = D.dump config source "dump.0-source-09-core.dct"
        , BST.configSinkResolve         = D.dump config source "dump.0-source-10-resolve.dct"
        , BST.configSinkPreCheck        = D.dump config source "dump.0-source-11-precheck.dct"
        , BST.configSinkCheckerTrace    = D.dump config source "dump.0-source-12-trace.txt"
        , BST.configSinkNamified        = D.dump config source "dump.0-source-13-namified.dct"
        , BST.configSinkChecked         = D.dump config source "dump.0-source-14-checked.dct"
        , BST.configSinkElaborated      = D.dump config source "dump.0-source-15-elaborated.dct"
        }


---------------------------------------------------------------------------------------------------
-- | Load and type-check a core tetra module.
tetraLoadText
        :: D.Config             -- ^ Driver config.
        -> B.Store              -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos CE.Name) CE.Name)

tetraLoadText config _store source str
 = B.coreLoad
        "TetraLoad"
        BE.fragment
        (if D.configInferTypes config then C.Synth [] else C.Recon)
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
 $ B.ConfigCoreLoad
        { B.configSinkTokens            = D.dump config source "dump.1-tetra-00-tokens.txt"
        , B.configSinkParsed            = D.dump config source "dump.1-tetra-01-parsed.dct"
        , B.configSinkChecked           = D.dump config source "dump.1-tetra-02-checked.dct"
        , B.configSinkTrace             = D.dump config source "dump.1-tetra-03-trace.txt"
        }


---------------------------------------------------------------------------------------------------
-- | Convert Core Tetra module to Core Salt.
tetraToSalt
        :: D.Config
        -> D.Source
        -> C.Module a CE.Name
        -> ExceptT [B.Error] IO (C.Module () CA.Name)

tetraToSalt config source mm
 = BCT.tetraToSalt
        (B.buildSpec $ D.configBuilder config)
        (D.configRuntime config)
        (CReannotate.reannotate (const ()) mm)
 $ BCT.ConfigTetraToSalt
        { BCT.configSinkExplicit        = D.dump config source "dump.1-tetra-02-explicit.dct"
        , BCT.configSinkLambdas         = D.dump config source "dump.1-tetra-03-lambdas.dct"
        , BCT.configSinkUnshare         = D.dump config source "dump.1-tetra-04-unshare.dct"
        , BCT.configSinkCurry           = D.dump config source "dump.1-tetra-05-curry.dct"
        , BCT.configSinkBoxing          = D.dump config source "dump.1-tetra-06-boxing.dct"
        , BCT.configSinkPrep            = D.dump config source "dump.1-tetra-07-prep.dct"
        , BCT.configSinkChecked         = D.dump config source "dump.1-tetra-08-checked.dct"
        , BCT.configSinkSalt            = D.dump config source "dump.2-salt-00-convert.dcs"
        }


---------------------------------------------------------------------------------------------------
-- | Convert a Core Tetra module to Shimmer code.
tetraToShimmer
        :: (Show a, Pretty a)
        => D.Config
        -> D.Source
        -> C.Module a CE.Name
        -> ExceptT [B.Error] IO (H.Module H.Name H.Name)

tetraToShimmer config source mm
 = BCT.tetraToShimmer
        mm
 $ BCT.ConfigTetraToShimmer
        { BCT.configSinkShimmer         = D.dump config source "dump.1-smr-01-convert.smr" }

