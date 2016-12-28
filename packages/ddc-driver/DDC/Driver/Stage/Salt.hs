
module DDC.Driver.Stage.Salt
        ( saltLoadText
        , saltSimplify
        , saltToSea

        , stageSaltToSlottedLLVM
        , stageSaltToUnSlottedLLVM
        , stageCompileSalt
        , stageCompileLLVM)
where
import Control.Monad.Trans.Except

import DDC.Driver.Dump
import DDC.Driver.Config                        as D
import DDC.Driver.Interface.Source              as D
import DDC.Build.Builder                        as B
import DDC.Build.Pipeline                       as B
import DDC.Core.Transform.Namify
import DDC.Data.Pretty
import System.FilePath
import Data.Maybe

import qualified DDC.Data.SourcePos             as SP

import DDC.Build.Interface.Store                as B

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Module                as C
import qualified DDC.Core.Simplifier.Recipe     as S
import qualified DDC.Core.Transform.Reannotate  as CReannotate

import qualified DDC.Core.Salt.Name             as A
import qualified DDC.Core.Salt.Profile          as A

import qualified DDC.Build.Stage.Core           as B
import qualified DDC.Build.Stage.Core.Salt      as BA
import qualified DDC.Build.Language.Salt        as BA

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
-- | Convert Core Salt to LLVM.
stageSaltToSlottedLLVM
        :: Config -> Source
        -> [PipeLlvm]
        -> PipeCore () A.Name

stageSaltToSlottedLLVM config source pipesLLVM
 = PipeCoreSimplify BA.fragment 0 normalizeSalt
   [ PipeCoreOutput       pprDefaultMode
                            (dump config source "dump.2-salt-03-normalized.dcs")
   , PipeCoreCheck          "SaltToSlottedLLVM" BA.fragment C.Recon SinkDiscard
     [ PipeCoreAsSalt
       [ PipeSaltSlotify
         [ PipeSaltOutput   (dump config source "dump.2-salt-04-slotify.dcs")
         , PipeSaltTransfer
           [ PipeSaltOutput (dump config source "dump.2-salt-05-transfer.dcs")
           , PipeSaltToLlvm (buildSpec $ configBuilder config)
                            pipesLLVM ]]]]]
 where  normalizeSalt
         = S.anormalize (makeNamifier A.freshT) 
                        (makeNamifier A.freshX)


stageSaltToUnSlottedLLVM
        :: Config -> Source
        -> [PipeLlvm]
        -> PipeCore () A.Name

stageSaltToUnSlottedLLVM config source pipesLLVM
 = PipeCoreSimplify BA.fragment 0 normalizeSalt
   [ PipeCoreCheck          "SaltToUnslottedLLVM" BA.fragment C.Recon SinkDiscard
     [ PipeCoreOutput        pprDefaultMode
                            (dump config source "dump.2-salt-03-normalized.dcs")
     , PipeCoreAsSalt
       [ PipeSaltTransfer
           [ PipeSaltOutput (dump config source "dump.2-salt-04-transfer.dcs")
           , PipeSaltToLlvm (buildSpec $ configBuilder config)
                            pipesLLVM ]]]]
 where  normalizeSalt
         = S.anormalize (makeNamifier A.freshT) 
                        (makeNamifier A.freshX)


---------------------------------------------------------------------------------------------------
-- | Compile Core Salt via C code.
stageCompileSalt
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Bool                 -- ^ Should we link this into an executable
        -> PipeCore () A.Name

stageCompileSalt config source filePath shouldLinkExe
 = let  -- Decide where to place the build products.
        (oPath, _)      = objectPathsOfConfig config filePath
        exePath         = exePathOfConfig    config filePath
        cPath           = replaceExtension   oPath  ".ddc.c"
   in
        PipeCoreSimplify        BA.fragment 0 normalizeSalt
         [ PipeCoreCheck        "CompileSalt" BA.fragment C.Recon SinkDiscard
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.2-salt-03-normalized.dcs")
           , PipeCoreAsSalt
             [ PipeSaltTransfer
               [ PipeSaltOutput (dump config source "dump.2-salt-04-transfer.dcs")
               , PipeSaltCompile
                        (buildSpec $ configBuilder config)
                        (configBuilder config)
                        cPath
                        oPath
                        (if shouldLinkExe 
                                then Just exePath 
                                else Nothing) 
                        (configKeepSeaFiles config)
                        ]]]]

 where  normalizeSalt
         = S.anormalize (makeNamifier A.freshT) 
                        (makeNamifier A.freshX)


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

