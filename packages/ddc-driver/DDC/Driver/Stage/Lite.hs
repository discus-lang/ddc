
module DDC.Driver.Stage.Lite
        ( stageLiteLoad
        , stageLiteOpt
        , stageLiteToSalt)
where
import DDC.Driver.Dump
import DDC.Driver.Config
import DDC.Interface.Source
import DDC.Build.Builder
import DDC.Build.Pipeline
import DDC.Core.Transform.Namify
import DDC.Base.Pretty
import qualified DDC.Core.Salt.Name             as Salt
import qualified DDC.Build.Language.Lite        as Lite
import qualified DDC.Core.Lite                  as Lite
import qualified DDC.Core.Check                 as C
import qualified DDC.Core.Simplifier.Recipe     as S


---------------------------------------------------------------------------------------------------
-- | Load and type check Core Lite.
stageLiteLoad
        :: Config -> Source
        -> [PipeCore () Lite.Name]
        -> PipeText Lite.Name Lite.Error

stageLiteLoad config source pipesLite
 = PipeTextLoadCore Lite.fragment 
        (if configInferTypes config then C.Synth else C.Recon)
                         (dump config source "dump.lite-check.txt")
 [ PipeCoreReannotate (const ())
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.lite-load.dcl")
        : pipesLite ) ]


---------------------------------------------------------------------------------------------------
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
        ( PipeCoreOutput pprDefaultMode
                         (dump config source "dump.lite-opt.dcl") 
        : pipes)


---------------------------------------------------------------------------------------------------
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
   [ PipeCoreCheck        Lite.fragment C.Recon SinkDiscard
     [ PipeCoreOutput     pprDefaultMode
                          (dump config source "dump.lite-normalized.dcl")
     , PipeCoreAsLite
       [ PipeLiteToSalt   (buildSpec $ configBuilder config) 
                          (configRuntime config)
         ( PipeCoreOutput pprDefaultMode
                          (dump config source "dump.salt.dcs")
         : pipesSalt)]]]
           
 where  normalizeLite
         = S.anormalize
                (makeNamifier Lite.freshT)      
                (makeNamifier Lite.freshX)
