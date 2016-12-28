
-- | Plumb driver configuration into the Salt specific build stages.
module DDC.Driver.Stage.Salt
        ( saltLoadText
        , saltSimplify
        , saltToSea
        , saltToLlvm
        , saltCompileViaSea

        , stageCompileLLVM)
where
import Control.Monad.Trans.Except

import DDC.Driver.Dump
import DDC.Driver.Config                        as D
import DDC.Driver.Interface.Source              as D
import DDC.Build.Builder                        as B
import DDC.Build.Pipeline                       as B
import DDC.Data.Pretty
import System.FilePath
import Data.Maybe

import qualified DDC.Data.SourcePos             as SP

import DDC.Build.Interface.Store                as B

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Transform.Reannotate  as CReannotate

import qualified DDC.Core.Salt.Name             as A

import qualified DDC.Build.Stage.Core           as B
import qualified DDC.Build.Stage.Core.Salt      as BA
import qualified DDC.Build.Language.Salt        as BA

import qualified DDC.Llvm.Syntax                as L


---------------------------------------------------------------------------------------------------
-- | Load and type-check a core tetra module.
saltLoadText 
        :: Config               -- ^ Driver config.
        -> B.Store              -- ^ Interface store.
        -> D.Source             -- ^ Source file meta-data.
        -> String               -- ^ Source file text.
        -> ExceptT [B.Error] IO
                   (C.Module (C.AnTEC SP.SourcePos A.Name) A.Name)

saltLoadText config _store source str
 = B.coreLoad
        "SaltLoad"
        BA.fragment
        (if configInferTypes config then C.Synth [] else C.Recon)
        (D.nameOfSource source)
        (D.lineStartOfSource source)
        str
 $ B.ConfigCoreLoad
        { B.configSinkTokens            = dump config source "dump.1-salt-00-tokens.txt"
        , B.configSinkParsed            = dump config source "dump.1-salt-01-parsed.dct"
        , B.configSinkChecked           = dump config source "dump.1-salt-02-checked.dct"
        , B.configSinkTrace             = dump config source "dump.1-salt-03-trace.txt" }


---------------------------------------------------------------------------------------------------
saltSimplify
        :: Config               -- ^ Driver config.
        -> D.Source             -- ^ Source file meta-data.
        -> C.Module a A.Name    -- ^ Module to simplify.
        -> ExceptT [B.Error] IO (C.Module () A.Name)

saltSimplify config _source mm
 = B.coreSimplify
        BA.fragment
        (0 :: Int)
        (configSimplSalt config)
        (CReannotate.reannotate (const ()) mm)


---------------------------------------------------------------------------------------------------
-- | Convert a Salt module to Sea text.
saltToSea
        :: (Show a, Pretty a)
        => Config               -- ^ Driver config.
        -> D.Source             -- ^ Source file meta data.
        -> C.Module a A.Name    -- ^ Module to convert.
        -> ExceptT [B.Error] IO String

saltToSea config source mm
 = BA.saltToSea
        (D.nameOfSource source)
        (buildSpec $ configBuilder config)
        mm


---------------------------------------------------------------------------------------------------
-- | Convert a Salt module to a LLVM module.
saltToLlvm 
        :: (Show a, Pretty a)
        => Config               -- ^ Driver config.
        -> D.Source             -- ^ Source file meta data.
        -> Bool                 -- ^ Whether to introduce stack slots.
        -> C.Module a A.Name    -- ^ Module to convert.
        -> ExceptT [B.Error] IO L.Module

saltToLlvm config source bAddSlots mm
 = BA.saltToLlvm
        (D.nameOfSource source)
        (buildSpec $ configBuilder config)
        bAddSlots
        (dump config source "dump.2-salt-03-normalized.dcs")
        (dump config source "dump.2-salt-04-slotify.dcs")
        (dump config source "dump.2-salt-05-transfer.dcs")
        mm


---------------------------------------------------------------------------------------------------
-- | Compile Core Salt code using the system C compiler.
saltCompileViaSea
        :: (Show a, Pretty a)
        => Config               -- ^ Driver config.
        -> D.Source             -- ^ Source file meta data.
        -> Bool                 -- ^ Whether we should link into an executable.
        -> C.Module a A.Name    -- ^ Module to convert.
        -> ExceptT [B.Error] IO ()

saltCompileViaSea config source bShouldLinkExe mm
 = do
        let (oPath, _)  = objectPathsOfConfig config (D.nameOfSource source)
        let mExePath    = if bShouldLinkExe
                                then Just $ exePathOfConfig config oPath
                                else Nothing

        BA.saltCompileViaSea
                (D.nameOfSource source)
                (configBuilder config)
                oPath mExePath Nothing 
                (configKeepSeaFiles config)
                mm


---------------------------------------------------------------------------------------------------
-- | Compile LLVM code.
stageCompileLLVM 
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Maybe [FilePath]     -- ^ If True then link with these other .os into an executable.
        -> PipeLlvm

stageCompileLLVM config _source filePath mOtherExeObjs
 = let  -- Decide where to place the build products.
        (oPath, _)      = objectPathsOfConfig config filePath
        exePath         = exePathOfConfig    config filePath
        llPath          = replaceExtension   oPath  ".ddc.ll"
        sPath           = replaceExtension   oPath  ".ddc.s"

   in   -- Make the pipeline for the final compilation.
        PipeLlvmCompile
          { pipeBuilder           = configBuilder config
          , pipeFileLlvm          = llPath
          , pipeFileAsm           = sPath
          , pipeFileObject        = oPath
          , pipeFileExe           = if isJust $ mOtherExeObjs then Just exePath else Nothing 
          , pipeLinkOtherObjects  = concat $ maybeToList mOtherExeObjs
          , pipeKeepLlvmFiles     = configKeepLlvmFiles config
          , pipeKeepAsmFiles      = configKeepAsmFiles  config }

