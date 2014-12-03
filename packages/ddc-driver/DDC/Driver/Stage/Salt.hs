
module DDC.Driver.Stage.Salt
        ( stageSaltLoad
        , stageSaltOpt
        , stageSaltToC
        , stageSaltToLLVM
        , stageCompileSalt
        , stageCompileLLVM)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Core.Transform.Namify
import DDC.Base.Pretty
import System.FilePath
import Data.Maybe
import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Core.Salt.Name             as Salt
import qualified DDC.Core.Salt.Convert          as Salt
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as S


---------------------------------------------------------------------------------------------------
-- | Load and type check a Core Salt module.
stageSaltLoad
        :: Config -> Source
        -> [PipeCore () Salt.Name]
        -> PipeText Salt.Name Salt.Error

stageSaltLoad config source pipesSalt
 = PipeTextLoadCore Salt.fragment 
        (if configInferTypes config then C.Synth else C.Recon)
        SinkDiscard
 [ PipeCoreReannotate (const ())
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.salt-load.dcl")
        : pipesSalt ) ]


---------------------------------------------------------------------------------------------------
-- | Optimise Core Salt.
stageSaltOpt
        :: Config -> Source
        -> [PipeCore () Salt.Name]
        -> PipeCore  () Salt.Name

stageSaltOpt config source pipes
 = PipeCoreSimplify 
        Salt.fragment
        (0 :: Int) 
        (configSimplSalt config)        
        ( PipeCoreOutput  pprDefaultMode 
                          (dump config source "dump.salt-opt.dcl")
        : pipes )


---------------------------------------------------------------------------------------------------
-- | Convert Core Salt to C code.
stageSaltToC
        :: Config -> Source
        -> Sink
        -> PipeCore () Salt.Name

stageSaltToC config source sink
 = PipeCoreSimplify       Salt.fragment 0 normalizeSalt
   [ PipeCoreCheck        Salt.fragment C.Recon SinkDiscard
     [ PipeCoreOutput     pprDefaultMode
                          (dump config source "dump.salt-normalized.dcs")
     , PipeCoreAsSalt
       [ PipeSaltTransfer
         [ PipeSaltOutput (dump config source "dump.salt-transfer.dcs")
         , PipeSaltPrint
                (not $ configSuppressHashImports config)
                (buildSpec $ configBuilder config)
                sink ]]]]

 where  normalizeSalt
         = S.anormalize (makeNamifier Salt.freshT) 
                        (makeNamifier Salt.freshX)


---------------------------------------------------------------------------------------------------
-- | Convert Core Salt to LLVM.
stageSaltToLLVM
        :: Config -> Source
        -> [PipeLlvm]
        -> PipeCore () Salt.Name

stageSaltToLLVM config source pipesLLVM
 = PipeCoreSimplify Salt.fragment 0 normalizeSalt
   [ PipeCoreCheck          Salt.fragment C.Recon SinkDiscard
     [ PipeCoreOutput       pprDefaultMode
                            (dump config source "dump.salt-normalized.dcs")
     , PipeCoreAsSalt
       [ PipeSaltTransfer
         [ PipeSaltOutput   (dump config source "dump.salt-transfer.dcs")
         , PipeSaltToLlvm   (buildSpec $ configBuilder config) 
                            pipesLLVM ]]]]

 where  normalizeSalt
         = S.anormalize (makeNamifier Salt.freshT) 
                        (makeNamifier Salt.freshX)


---------------------------------------------------------------------------------------------------
-- | Compile Core Salt via C code.
stageCompileSalt
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Bool                 -- ^ Should we link this into an executable
        -> PipeCore () Salt.Name

stageCompileSalt config source filePath shouldLinkExe
 = let  -- Decide where to place the build products.
        oPath   = objectPathOfConfig config filePath
        exePath = exePathOfConfig    config filePath
        cPath   = replaceExtension   oPath  ".ddc.c"
   in
        PipeCoreSimplify        Salt.fragment 0 normalizeSalt
         [ PipeCoreCheck        Salt.fragment C.Recon SinkDiscard
           [ PipeCoreOutput     pprDefaultMode
                                (dump config source "dump.salt-normalized.dcs")
           , PipeCoreAsSalt
             [ PipeSaltTransfer
               [ PipeSaltOutput (dump config source "dump.salt-transfer.dcs")
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
         = S.anormalize (makeNamifier Salt.freshT) 
                        (makeNamifier Salt.freshX)


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
        oPath   = objectPathOfConfig config filePath
        exePath = exePathOfConfig    config filePath
        llPath  = replaceExtension   oPath  ".ddc.ll"
        sPath   = replaceExtension   oPath  ".ddc.s"

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

