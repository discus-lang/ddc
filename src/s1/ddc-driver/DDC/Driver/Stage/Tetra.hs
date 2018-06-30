
-- | Plumb driver configuration into the Tetra specific build stages.
module DDC.Driver.Stage.Tetra
        ( sourceLoadText
        , discusLoadText
        , discusToSalt)
where
import Control.Monad.Trans.Except

import qualified DDC.Driver.Dump                as D
import qualified DDC.Driver.Config              as D
import qualified DDC.Driver.Interface.Source    as D

import qualified DDC.Build.Interface.Store      as B
import qualified DDC.Build.Pipeline.Error       as B
import qualified DDC.Build.Builder              as B
import qualified DDC.Build.Stage.Core           as B
import qualified DDC.Build.Language.Discus      as BE
import qualified DDC.Build.Stage.Source.Discus  as BSD
import qualified DDC.Build.Stage.Core.Discus    as BCD

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Discus                as CE
import qualified DDC.Core.Salt                  as CA
import qualified DDC.Core.Transform.Reannotate  as CReannotate

import qualified DDC.Data.SourcePos             as SP


---------------------------------------------------------------------------------------------------
-- | Load and type-check a source Discus module.
sourceLoadText
        :: D.Config             -- ^ Driver config.
        -> B.Store CE.Name      -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos CE.Name) CE.Name)

sourceLoadText config store source str
 = BSD.sourceLoad
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
        store
 $ BSD.ConfigLoadSourceTetra
        { BSD.configSinkTokens          = D.dump config source "dump.0-source-01-tokens.txt"
        , BSD.configSinkParsed          = D.dump config source "dump.0-source-02-parsed.ds"
        , BSD.configSinkFresh           = D.dump config source "dump.0-source-03-fresh.ds"
        , BSD.configSinkDefix           = D.dump config source "dump.0-source-04-defix.ds"
        , BSD.configSinkExpand          = D.dump config source "dump.0-source-05-expand.ds"
        , BSD.configSinkGuards          = D.dump config source "dump.0-source-06-guards.ds"
        , BSD.configSinkMatches         = D.dump config source "dump.0-source-07-matches.ds"
        , BSD.configSinkPrep            = D.dump config source "dump.0-source-08-prep.ds"
        , BSD.configSinkCore            = D.dump config source "dump.0-source-09-core.dcd"
        , BSD.configSinkImport          = D.dump config source "dump.0-source-10-import.dcd"
        , BSD.configSinkPreCheck        = D.dump config source "dump.0-source-11-precheck.dcd"
        , BSD.configSinkCheckerTrace    = D.dump config source "dump.0-source-12-trace.txt"
        , BSD.configSinkNamified        = D.dump config source "dump.0-source-13-namified.dcd"
        , BSD.configSinkChecked         = D.dump config source "dump.0-source-14-checked.dcd"
        , BSD.configSinkElaborated      = D.dump config source "dump.0-source-15-elaborated.dcd"
        , BSD.configSinkExposed         = D.dump config source "dump.0-source-16-exposed.dcd"
        }


---------------------------------------------------------------------------------------------------
-- | Load and type-check a Core Discus module.
discusLoadText
        :: D.Config             -- ^ Driver config.
        -> B.Store CE.Name      -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos CE.Name) CE.Name)

discusLoadText config _store source str
 = B.coreLoad
        "DiscusLoad"
        BE.fragment
        (if D.configInferTypes config then C.Synth [] else C.Recon)
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
 $ B.ConfigCoreLoad
        { B.configSinkTokens            = D.dump config source "dump.1-discus-00-tokens.txt"
        , B.configSinkParsed            = D.dump config source "dump.1-discus-01-parsed.ds"
        , B.configSinkChecked           = D.dump config source "dump.1-discus-02-checked.dcd"
        , B.configSinkTrace             = D.dump config source "dump.1-discus-03-trace.txt"
        }


---------------------------------------------------------------------------------------------------
-- | Convert Core Discus module to Core Salt.
discusToSalt
        :: D.Config
        -> D.Source
        -> [C.ModuleName]
        -> C.Module a CE.Name
        -> ExceptT [B.Error] IO (C.Module () CA.Name)

discusToSalt config source mnsInit mm
 = BCD.discusToSalt
        (B.buildSpec $ D.configBuilder config)
        (D.configRuntime config)
        mnsInit
        (CReannotate.reannotate (const ()) mm)
 $ BCD.ConfigDiscusToSalt
        { BCD.configSinkExplicit        = D.dump config source "dump.1-discus-02-explicit.dcd"
        , BCD.configSinkInitialize      = D.dump config source "dump.1-discus-03.initialize.dcd"
        , BCD.configSinkLambdas         = D.dump config source "dump.1-discus-04-lambdas.dcd"
        , BCD.configSinkUnshare         = D.dump config source "dump.1-discus-05-unshare.dcd"
        , BCD.configSinkCurry           = D.dump config source "dump.1-discus-06-curry.dcd"
        , BCD.configSinkBoxing          = D.dump config source "dump.1-discus-07-boxing.dcd"
        , BCD.configSinkPrep            = D.dump config source "dump.1-discus-08-prep.dcd"
        , BCD.configSinkChecked         = D.dump config source "dump.1-discus-09-checked.dcd"
        , BCD.configSinkSalt            = D.dump config source "dump.2-salt-00-convert.dcs"
        }

