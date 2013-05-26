-- | Compiler stages.
--
--     A compiler stage is a sequence of standard transformations.
--     Each of the individual transformations are expressed as a pipeline from 
--     "DDC.Build.Pipeline". The stages here run several pipelines each,
--     and contain the code that can dump the intermediate program after
--     each transformation.
--
module DDC.Driver.Stage
        ( Config        (..)
        , ViaBackend    (..)

          -- * Flow stages
        , stageFlowLoad
        , stageFlowPrep
        , stageFlowLower

          -- * Lite stages
        , stageLiteLoad
        , stageLiteOpt
        , stageLiteToSalt

          -- * Salt stages
        , stageSaltOpt
        , stageSaltToC
        , stageSaltToLLVM
        , stageCompileSalt

          -- * LLVM stages
        , stageCompileLLVM)
where
import DDC.Driver.Source
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Core.Transform.Namify
import DDC.Core.Simplifier                      (Simplifier)
import System.FilePath
import Data.Monoid
import Data.Maybe

import qualified DDC.Core.Flow                  as Flow
import qualified DDC.Core.Flow.Profile          as Flow
import qualified DDC.Build.Language.Flow        as Flow

import qualified DDC.Build.Language.Salt        as Salt
import qualified DDC.Core.Salt.Runtime          as Salt
import qualified DDC.Core.Salt.Name             as Salt

import qualified DDC.Build.Language.Lite        as Lite
import qualified DDC.Core.Lite                  as Lite

import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier            as S
import qualified DDC.Core.Simplifier.Recipe     as S
import qualified DDC.Core.Transform.Namify      as S
import qualified DDC.Core.Transform.Snip        as Snip


-- | Configuration for main compiler stages.
data Config
        = Config
        { -- | Dump intermediate code.
          configDump                    :: Bool

          -- | Simplifiers to apply to intermediate code
        , configSimplLite               :: Simplifier Int () Lite.Name
        , configSimplSalt               :: Simplifier Int () Salt.Name

          -- | Backend code generator to use
        , configViaBackend              :: ViaBackend

          -- | Runtime system configuration
        , configRuntime                 :: Salt.Config

          -- | The builder to use for the target architecture
        , configBuilder                 :: Builder

          -- | Suppress imports in Core modules
        , configSuppressCoreImports     :: Bool

          -- | Suppress the #import prelude in C modules
        , configSuppressHashImports     :: Bool 

          -- | Override output file
        , configOutputFile              :: Maybe FilePath

          -- | Override directory for build products
        , configOutputDir               :: Maybe FilePath

          -- | Keep intermediate .ddc.ll files
        , configKeepLlvmFiles           :: Bool

          -- | Keep intermediate .ddc.c files
        , configKeepSeaFiles            :: Bool

          -- | Keep intermediate .ddc.s files
        , configKeepAsmFiles            :: Bool
        }


data ViaBackend
        -- | Compile via the C backend.
        = ViaC

        -- | Compile via the LLVM backend.
        | ViaLLVM
        deriving Show


-------------------------------------------------------------------------------
-- | Type check Core Flow.
stageFlowLoad
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        -> PipeText Flow.Name Flow.Error

stageFlowLoad config source pipesFlow
 = PipeTextLoadCore Flow.fragment
 [ PipeCoreReannotate (const ())
        ( PipeCoreOutput (dump config source "dump.flow.dcf")
        : pipesFlow ) ]


-------------------------------------------------------------------------------
-- | Prepare a Core Flow module for lowering.
stageFlowPrep
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore () Flow.Name

stageFlowPrep config source pipesFlow
 = PipeCoreReannotate   (const ())
 [ PipeCoreSimplify     Flow.fragment (0 :: Int) simplNorm
   [ PipeCoreOutput     (dump config source "dump.lower-norm.dcf")
   , PipeCoreAsFlow
     [ PipeFlowPrep
       ( PipeCoreOutput (dump config source "dump.lower-prep.dcf")
         : pipesFlow)]]]
 
 where  simplNamify
         =  S.Trans (S.Namify namifierT namifierX)

        simplNorm
         =  S.Trans (S.Snip $ Snip.configZero { Snip.configSnipLetBody = True })
         <> S.flatten 
         <> simplNamify

        namifierT       = S.makeNamifier Flow.freshT
        namifierX       = S.makeNamifier Flow.freshX


-------------------------------------------------------------------------------
-- | Lower a Core Flow module.
--   Is needs to already be prepped,
--   and have full type annotations.
stageFlowLower
        :: Config -> Source
        -> [PipeCore () Flow.Name]
        ->  PipeCore (C.AnTEC () Flow.Name) Flow.Name

stageFlowLower config source pipesFlow
 = PipeCoreAsFlow
     [ PipeFlowLower
       ( PipeCoreOutput    (dump config source "dump.lower-done.dcf")
       : pipesFlow ) ]


-------------------------------------------------------------------------------
-- | Type check Core Lite.
stageLiteLoad
        :: Config -> Source
        -> [PipeCore () Lite.Name]
        -> PipeText Lite.Name Lite.Error

stageLiteLoad config source pipesLite
 = PipeTextLoadCore Lite.fragment
 [ PipeCoreReannotate (const ())
        ( PipeCoreOutput (dump config source "dump.lite.dcl")
        : pipesLite ) ]


-------------------------------------------------------------------------------
-- | Optimise Core Lite.
stageLiteOpt 
        :: Config -> Source
        -> [PipeCore () Lite.Name]
        ->  PipeCore () Lite.Name

stageLiteOpt config source pipes
 = PipeCoreSimplify 
        Lite.fragment
        (0 :: Int) 
        (configSimplLite config)
        ( PipeCoreOutput (dump config source "dump.lite-opt.dcl") 
        : pipes)


-------------------------------------------------------------------------------
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
        ( PipeCoreOutput  (dump config source "dump.salt-opt.dcl")
        : pipes )


-------------------------------------------------------------------------------
-- | Convert Core Lite to Core Salt.
---
--   The Lite to Salt transform requires the program to be normalised,
--   and have type annotations.
stageLiteToSalt 
        :: Config -> Source
        -> [PipeCore () Salt.Name] 
        -> PipeCore  () Lite.Name

stageLiteToSalt config source pipesSalt
 = PipeCoreSimplify       Lite.fragment 0 normalizeLite
   [ PipeCoreCheck        Lite.fragment
     [ PipeCoreOutput     (dump config source "dump.lite-normalized.dcl")
     , PipeCoreAsLite
       [ PipeLiteToSalt   (buildSpec $ configBuilder config) 
                          (configRuntime config)
         ( PipeCoreOutput (dump config source "dump.salt.dcs")
         : pipesSalt)]]]
           
 where  normalizeLite
         = S.anormalize
                (makeNamifier Lite.freshT)      
                (makeNamifier Lite.freshX)


-------------------------------------------------------------------------------
-- | Convert Core Salt to C code.
stageSaltToC
        :: Config -> Source
        -> Sink
        -> PipeCore () Salt.Name

stageSaltToC config source sink
 = PipeCoreSimplify       Salt.fragment 0 normalizeSalt
   [ PipeCoreCheck        Salt.fragment
     [ PipeCoreOutput     (dump config source "dump.salt-normalized.dcs")
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


-------------------------------------------------------------------------------
-- | Compile Core Salt via C code.
stageCompileSalt
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Bool                 -- ^ Should we link this into an executable
        -> PipeCore () Salt.Name

stageCompileSalt config source filePath shouldLinkExe
 = let  -- Decide where to place the build products.
        outputDir      = fromMaybe (takeDirectory filePath) (configOutputDir config)
        outputDirBase  = dropExtension (replaceDirectory filePath outputDir)
        cPath          = outputDirBase ++ ".ddc.c"
        oPath          = outputDirBase ++ ".o"
        exePathDefault = outputDirBase
        exePath        = fromMaybe exePathDefault (configOutputFile config)
   in
        PipeCoreSimplify        Salt.fragment 0 normalizeSalt
         [ PipeCoreCheck        Salt.fragment
           [ PipeCoreOutput     (dump config source "dump.salt-normalized.dcs")
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


-------------------------------------------------------------------------------
-- | Convert Core Salt to LLVM.
stageSaltToLLVM
        :: Config -> Source
        -> [PipeLlvm]
        -> PipeCore () Salt.Name

stageSaltToLLVM config source pipesLLVM
 = PipeCoreSimplify Salt.fragment 0 normalizeSalt
   [ PipeCoreCheck          Salt.fragment
     [ PipeCoreOutput       (dump config source "dump.salt-normalized.dcs")
     , PipeCoreAsSalt
       [ PipeSaltTransfer
         [ PipeSaltOutput   (dump config source "dump.salt-transfer.dcs")
         , PipeSaltToLlvm   (buildSpec $ configBuilder config) 
                            pipesLLVM ]]]]

 where  normalizeSalt
         = S.anormalize (makeNamifier Salt.freshT) 
                        (makeNamifier Salt.freshX)


-------------------------------------------------------------------------------
-- | Compile LLVM code.
stageCompileLLVM 
        :: Config -> Source
        -> FilePath             -- ^ Path of original source file.
                                --   Build products are placed into the same dir.
        -> Bool                 -- ^ Should we link this into an executable
        -> PipeLlvm

stageCompileLLVM config _source filePath shouldLinkExe
 = let  -- Decide where to place the build products.
        outputDir      = fromMaybe (takeDirectory filePath) (configOutputDir config)
        outputDirBase  = dropExtension (replaceDirectory filePath outputDir)
        llPath         = outputDirBase ++ ".ddc.ll"
        sPath          = outputDirBase ++ ".ddc.s"
        oPath          = outputDirBase ++ ".o"
        exePathDefault = outputDirBase
        exePath        = fromMaybe exePathDefault (configOutputFile config)
   in   -- Make the pipeline for the final compilation.
        PipeLlvmCompile
          { pipeBuilder           = configBuilder config
          , pipeFileLlvm          = llPath
          , pipeFileAsm           = sPath
          , pipeFileObject        = oPath
          , pipeFileExe           = if shouldLinkExe 
                                        then Just exePath 
                                        else Nothing 
          , pipeKeepLlvmFiles     = configKeepLlvmFiles config
          , pipeKeepAsmFiles      = configKeepAsmFiles  config }


------------------------------------------------------------------------------
-- | If the Dump mode is set 
--    then produce a SinkFile to write a module to a file, 
--    otherwise produce SinkDiscard to drop it on the floor.
dump :: Config -> Source -> String -> Sink
dump config source dumpFile 
        | configDump config
        = let   outputDir
                 | SourceFile filePath  <- source
                 = fromMaybe (takeDirectory filePath) 
                             (configOutputDir config)

                 | otherwise
                 = fromMaybe "."
                             (configOutputDir config)

          in    SinkFile $ outputDir </> dumpFile

        | otherwise
        = SinkDiscard

