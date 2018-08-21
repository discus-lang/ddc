
-- | Plumb driver configuration into the Salt specific build stages.
module DDC.Driver.Stage.Salt
        ( saltLoadText
        , saltSimplify
        , saltToLlvm
        , saltCompileViaLlvm)
where
import Control.Monad.Trans.Except
import Control.DeepSeq

import DDC.Data.Pretty
import qualified DDC.Data.SourcePos                     as SP

import qualified DDC.Driver.Dump                        as D
import qualified DDC.Driver.Config                      as D
import qualified DDC.Driver.Interface.Source            as D

import qualified DDC.Build.Builder                      as B
import qualified DDC.Build.Pipeline                     as B
import qualified DDC.Build.Stage.Core                   as B
import qualified DDC.Build.Stage.Core.Salt              as BA
import qualified DDC.Build.Language.Salt                as BA

import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Transform.Reannotate          as CReannotate

import qualified DDC.Core.Salt.Name                     as A

import qualified DDC.Llvm.Syntax                        as L


---------------------------------------------------------------------------------------------------
-- | Load and type-check a Core Salt module.
saltLoadText
        :: D.Config             -- ^ Driver config.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos A.Name) A.Name)

saltLoadText config source str
 = B.coreLoad
        "SaltLoad"
        BA.fragment Nothing
        (if D.configInferTypes config then C.Synth [] else C.Recon)
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
 $ B.ConfigCoreLoad
        { B.configSinkTokens            = D.dump config source "dump.1-salt-00-tokens.txt"
        , B.configSinkParsed            = D.dump config source "dump.1-salt-01-parsed.dct"
        , B.configSinkChecked           = D.dump config source "dump.1-salt-02-checked.dct"
        , B.configSinkTrace             = D.dump config source "dump.1-salt-03-trace.txt" }


---------------------------------------------------------------------------------------------------
-- | Simplify a Core Salt module.
saltSimplify
        :: D.Config             -- ^ Driver config.
        -> D.Source             -- ^ Source file meta-data.
        -> C.Module a A.Name    -- ^ Module to simplify.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

saltSimplify config _source mm
 = B.coreSimplify
        BA.fragment
        (0 :: Int)
        (D.configSimplSalt config)
        (CReannotate.reannotate (const ()) mm)


---------------------------------------------------------------------------------------------------
-- | Convert a Salt module to a LLVM module.
saltToLlvm
        :: (NFData a, Show a, Pretty a)
        => D.Config             -- ^ Driver config.
        -> D.Source             -- ^ Source file meta data.
        -> Bool                 -- ^ Whether to introduce stack slots.
        -> C.Module a A.Name    -- ^ Module to convert.
        -> ExceptT [B.Error] IO L.Module

saltToLlvm config source bAddSlots mm
 = BA.saltToLlvm
        (D.nameOfSource source)
        (B.buildSpec $ D.configBuilder config)
        bAddSlots
        (D.dump config source "dump.2-salt-03-normalized.dcs")
        (D.dump config source "dump.2-salt-04-slotify.dcs")
        (D.dump config source "dump.2-salt-05-transfer.dcs")
        mm


---------------------------------------------------------------------------------------------------
-- | Compile a Core Salt module using the system Llvm compiler.
saltCompileViaLlvm
        :: (NFData a, Show a, Pretty a)
        => D.Config             -- ^ Driver config.
        -> D.Source             -- ^ Source file meta data.
        -> Maybe [FilePath]     -- ^ Link with these other .o files.
        -> Bool                 -- ^ Whether to add stack slot management code.
        -> Bool                 -- ^ Whether we should link an executable.
        -> C.Module a A.Name    -- ^ Module to convert.
        -> ExceptT [B.Error] IO ()

saltCompileViaLlvm config source mOtherExeObjs
        bSlotify bShouldLinkExe mm
 = do
        let (oPath, _)
                = D.objectPathsOfConfig config (D.nameOfSource source)

        let mExePath
                = if bShouldLinkExe
                        then Just $ D.exePathOfConfig config (D.nameOfSource source)
                        else Nothing

        BA.saltCompileViaLlvm
                (D.nameOfSource source)
                (D.configBuilder config)
                oPath
                mExePath
                mOtherExeObjs
                bSlotify
                (D.configKeepLlvmFiles config)
                (D.configKeepAsmFiles  config)
                (D.dump config source "dump.2-salt-06-prep.dcs")
                (D.dump config source "dump.2-salt-07-slotify.dcs")
                (D.dump config source "dump.2-salt-07-transfer.dcs")
                mm

