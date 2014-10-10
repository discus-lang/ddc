{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Core
        ( PipeCore (..)
        , pipeCore
        , pipeCores

        , PipeTetra (..)
        , pipeTetra

        , PipeLite (..)
        , pipeLite

        , PipeFlow (..)
        , pipeFlow)
where
import DDC.Build.Pipeline.Error
import DDC.Build.Pipeline.Sink
import DDC.Build.Pipeline.Salt
import DDC.Build.Language
import DDC.Core.Simplifier
import DDC.Base.Pretty
import DDC.Base.Name
import DDC.Data.Canned
import DDC.Llvm.Pretty                                  ()

import qualified DDC.Core.Flow                          as Flow
import qualified DDC.Core.Flow.Profile                  as Flow
import qualified DDC.Core.Flow.Transform.Forward        as Flow
import qualified DDC.Core.Flow.Transform.Melt           as Flow
import qualified DDC.Core.Flow.Transform.Wind           as Flow
import qualified DDC.Core.Flow.Transform.Rates.SeriesOfVector as Flow
import qualified DDC.Core.Flow.Convert                  as Flow

import qualified DDC.Core.Tetra.Transform.Curry         as Tetra
import qualified DDC.Core.Tetra.Transform.Boxing        as Tetra
import qualified DDC.Core.Tetra                         as Tetra

import qualified DDC.Core.Lite                          as Lite

import qualified DDC.Core.Salt.Platform                 as Salt
import qualified DDC.Core.Salt.Runtime                  as Salt
import qualified DDC.Core.Salt                          as Salt

import qualified DDC.Core.Transform.AnonymizeX          as C
import qualified DDC.Core.Transform.Reannotate          as C
import qualified DDC.Core.Transform.Namify              as C
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Flatten             as Flatten
import qualified DDC.Core.Transform.Eta                 as Eta
import qualified DDC.Core.Transform.Beta                as Beta
import qualified DDC.Core.Transform.Lambdas             as Lambdas
import qualified DDC.Core.Transform.TransformModX       as TMod
import qualified DDC.Core.Transform.Forward             as Forward
import qualified DDC.Core.Simplifier                    as C

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Pretty                        as C
import qualified DDC.Core.Module                        as C

import qualified DDC.Core.Exp                           as C
import qualified DDC.Core.Compounds                     as C

import qualified DDC.Type.Env                           as Env

import qualified Control.Monad.State.Strict             as S
import Control.Monad
import Control.DeepSeq


-- | Process a core module.
data PipeCore a n where
  -- Plumb the module on without transforming it.
  PipeCoreId
        :: ![PipeCore a n]
        -> PipeCore a n

  -- Output a module to console or file.
  PipeCoreOutput    
        :: !(C.PrettyMode (C.Module a n))
        -> !Sink 
        -> PipeCore a n

  -- Type check a module.
  PipeCoreCheck      
        :: (Pretty a, Pretty (err (C.AnTEC a n)))
        => !(Fragment n err)            -- Language fragment to check against.
        -> !(C.Mode n)                  -- Checker mode.
        -> !Sink                        -- Sink for checker trace.
        -> ![PipeCore (C.AnTEC a n) n]  -- Pipes for result.
        -> PipeCore a n

  -- Type check a module, discarding previous per-node type annotations.
  PipeCoreReCheck
        :: (NFData a, Show a, Pretty a, Pretty (err (C.AnTEC a n)))
        => !(Fragment n err)
        -> !(C.Mode n)
        -> ![PipeCore (C.AnTEC a n)  n]
        -> PipeCore  (C.AnTEC a n') n

  -- Reannotate a module module.
  PipeCoreReannotate
        :: (NFData b, Show b)
        => (a -> b)
        -> ![PipeCore b n]
        ->  PipeCore  a n

  -- Apply a simplifier to a module.
  PipeCoreSimplify  
        :: (Pretty a, CompoundName n)
        => !(Fragment n err)
        -> !s
        -> !(Simplifier s a n)
        -> ![PipeCore () n] 
        -> PipeCore a n

  -- Treat a module as belonging to the Core Tetra fragment from now on.
  PipeCoreAsTetra
        :: ![PipeTetra a]
        -> PipeCore a Tetra.Name

  -- Treat a module as belonging to the Core Lite fragment from now on.
  PipeCoreAsLite
        :: ![PipeLite]
        -> PipeCore (C.AnTEC () Lite.Name) Lite.Name

  -- Treat a module as belonging to the Core Flow fragment from now on.
  PipeCoreAsFlow 
        :: Pretty a
        => ![PipeFlow a]
        -> PipeCore a Flow.Name

  -- Treat a module as belonging to the Core Salt fragment from now on.
  PipeCoreAsSalt
        :: Pretty a 
        => ![PipeSalt a] 
        -> PipeCore a Salt.Name

  -- Apply a canned function to a module.
  -- This is helpful for debugging, and tweaking the output before pretty printing.
  -- More reusable transforms should be made into their own pipeline stage.
  PipeCoreHacks
        :: (NFData a, Show b, NFData b)
        => Canned (C.Module a n -> IO (C.Module b n))
        -> ![PipeCore b n]
        -> PipeCore a n


-- | Process a Core module.
--
--   Returns empty list on success.
pipeCore
        :: (NFData a, Show a, NFData n, Eq n, Ord n, Show n, Pretty n)
        => C.Module a n
        -> PipeCore a n
        -> IO [Error]

pipeCore !mm !pp
 = case pp of
        PipeCoreId !pipes
         -> {-# SCC "PipeCoreId" #-}
            pipeCores mm pipes

        PipeCoreOutput !mode !sink
         -> {-# SCC "PipeCoreOutput" #-}
            pipeSink (renderIndent $ pprModePrec mode 0 mm) sink

        PipeCoreCheck !fragment !mode !sinkTrace !pipes
         -> {-# SCC "PipeCoreCheck" #-}
            let profile         = fragmentProfile fragment

                -- Check the module is type correct, 
                --  using the generic core type checker.
                goCheck mm1
                 = case C.checkModule (C.configOfProfile profile) mm1 mode of
                        (Left err,  C.CheckTrace doc) 
                         -> do  pipeSink (renderIndent doc) sinkTrace
                                return [ErrorLint err]
                        
                        (Right mm2, C.CheckTrace doc) 
                         -> do  pipeSink (renderIndent doc) sinkTrace
                                goComplies mm2

                -- Check the module compiles with the language profile.
                goComplies mm1
                 = case C.complies profile mm1 of
                        Just err         -> return [ErrorLint err]
                        Nothing          -> goFragment mm1

                -- Check the module satisfies fragment specific checks.
                goFragment mm1
                 = case fragmentCheckModule fragment mm1 of
                        Just err         -> return [ErrorLint err]
                        Nothing          -> pipeCores mm1 pipes

             in goCheck mm

        PipeCoreReCheck !fragment !mode !pipes
         -> {-# SCC "PipeCoreReCheck" #-}
            pipeCore (C.reannotate C.annotTail mm)
         $  PipeCoreCheck fragment mode SinkDiscard pipes 

        PipeCoreReannotate f !pipes
         -> {-# SCC "PipeCoreStrip" #-}
            let mm' = (C.reannotate f mm)
            in  pipeCores mm' pipes

        PipeCoreSimplify !fragment !nameZero !simpl !pipes
         -> {-# SCC "PipeCoreSimplify" #-}
            let profile         = fragmentProfile fragment
                primKindEnv     = C.profilePrimKinds      profile
                primTypeEnv     = C.profilePrimTypes      profile

                !mm'            = (result . flip S.evalState nameZero
                                   $ applySimplifier profile primKindEnv primTypeEnv simpl mm)

                !mm2            = C.reannotate (const ()) mm'

                -- NOTE: It is helpful to deepseq here so that we release 
                --       references to the unsimplified version of the code.
                --       Because we've just applied reannotate, we also
                --       release type annotations on the expression tree.
            in  mm2 `deepseq` pipeCores mm2 pipes

        PipeCoreAsTetra !pipes
         -> {-# SCC "PipeCoreAsTetra" #-}
            liftM concat $ mapM (pipeTetra mm) pipes

        PipeCoreAsLite !pipes
         -> {-# SCC "PipeCoreAsLite" #-}
            liftM concat $ mapM (pipeLite mm) pipes

        PipeCoreAsFlow !pipes
         -> {-# SCC "PipeCoreAsFlow" #-}
            liftM concat $ mapM (pipeFlow mm) pipes

        PipeCoreAsSalt !pipes
         -> {-# SCC "PipeCoreAsSalt" #-}
            liftM concat $ mapM (pipeSalt mm) pipes

        PipeCoreHacks !(Canned f) !pipes
         -> {-# SCC "PipeCoreHacks" #-} 
            do  mm'     <- f mm
                pipeCores mm' pipes


pipeCores :: (NFData a, Show a, NFData n, Eq n, Ord n, Show n, Pretty n)
          => C.Module a n -> [PipeCore a n] -> IO [Error]

pipeCores !mm !pipes 
 = go [] pipes
 where  go !errs []   
         = return errs

        go !errs (pipe : rest)
         = do   !err     <- pipeCore mm pipe
                go (errs ++ err) rest


-- PipeTetra ------------------------------------------------------------------
-- | Process a Core Tetra module.
data PipeTetra a where
        -- Output the module in core language syntax.
        PipeTetraOutput 
         :: !Sink
         -> PipeTetra a

        -- Manage currying of functions.
        PipeTetraCurry
         :: (NFData a, Show a)
         => ![PipeCore (C.AnTEC a Tetra.Name) Tetra.Name]
         -> PipeTetra  (C.AnTEC a Tetra.Name)

        -- Manage boxing of numeric values.
        PipeTetraBoxing
         :: (NFData a, Show a)
         => ![PipeCore a Tetra.Name]
         -> PipeTetra a

        -- Convert the module to the Core Salt Fragment.
        PipeTetraToSalt 
         :: (NFData a, Show a)
         => !Salt.Platform 
         -> !Salt.Config
         -> ![PipeCore a Salt.Name]
         -> PipeTetra  (C.AnTEC a Tetra.Name)


-- | Process a Core Tetra module.
pipeTetra 
        :: C.Module a Tetra.Name
        -> PipeTetra a
        -> IO [Error]

pipeTetra !mm !pp
 = case pp of
        PipeTetraOutput !sink
         -> {-# SCC "PipeTetraOutput" #-}
            pipeSink (renderIndent $ ppr mm)  sink

        PipeTetraBoxing !pipes
         -> {-# SCC "PipeTetraBoxing" #-}
            pipeCores (Tetra.boxingModule mm) pipes

        PipeTetraCurry  !pipes
         -> {-# SCC "PipeTetraCurry"  #-}
            pipeCores (Tetra.curryModule mm)  pipes

        PipeTetraToSalt !platform !runConfig !pipes
         -> {-# SCC "PipeTetraToSalt" #-}
            case Tetra.saltOfTetraModule platform runConfig 
                        (C.profilePrimDataDefs Tetra.profile) 
                        (C.profilePrimKinds    Tetra.profile)
                        (C.profilePrimTypes    Tetra.profile)
                        mm 
             of  Left  err  -> return [ErrorTetraConvert err]
                 Right mm'  -> pipeCores mm' pipes 



-- PipeLite -------------------------------------------------------------------
-- | Process a Core Lite module.
data PipeLite
        -- | Output the module in core language syntax.
        = PipeLiteOutput !Sink

        -- | Convert the module to the Core Salt Fragment.
        | PipeLiteToSalt !Salt.Platform 
                         !Salt.Config
                         ![PipeCore () Salt.Name]


-- | Process a Core Lite module.
pipeLite :: C.Module (C.AnTEC () Lite.Name) Lite.Name
         -> PipeLite
         -> IO [Error]

pipeLite !mm !pp
 = case pp of
        PipeLiteOutput !sink
         -> {-# SCC "PipeLiteOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeLiteToSalt !platform !runConfig !pipes
         -> {-# SCC "PipeLiteToSalt" #-}
            case Lite.saltOfLiteModule platform runConfig 
                        (C.profilePrimDataDefs Lite.profile) 
                        (C.profilePrimKinds    Lite.profile)
                        (C.profilePrimTypes    Lite.profile)
                        mm 
             of  Left  err  -> return [ErrorLiteConvert err]
                 Right mm'  -> pipeCores mm' pipes 


-- PipeFlow -------------------------------------------------------------------
-- | Process a Core Flow module.
data PipeFlow a where
  -- Output the module in core language syntax.
  PipeFlowOutput 
        :: Sink
        -> PipeFlow a

  -- Apply a canned function to a module.
  -- This is helpful for debugging, and tweaking the output before pretty printing.
  -- More reusable transforms should be made into their own pipeline stage.
  PipeFlowHacks
        :: (NFData a, Show b, NFData b)
        => Canned (C.Module a Flow.Name -> IO (C.Module b Flow.Name))
        -> ![PipeFlow b]
        -> PipeFlow a

  -- Run the prep transform to expose flow operators.
  PipeFlowPrep
        :: [PipeCore () Flow.Name] 
        -> PipeFlow ()

  -- Run rate inference to transform vector operations into loops of series expressions.
  PipeFlowRate
        :: [PipeCore () Flow.Name] 
        -> PipeFlow ()

  -- Run the lowering transform on a module.
  --  It needs to be already prepped and have full type annotations.
  --  Lowering it kills the annotations.
  PipeFlowLower
        :: Flow.Config
        -> [PipeCore () Flow.Name]
        -> PipeFlow (C.AnTEC () Flow.Name)

  -- Melt compound data into primitive types.
  PipeFlowMelt
        :: [PipeCore () Flow.Name]
        -> PipeFlow (C.AnTEC () Flow.Name)

  -- Wind loop# primops into tail recursive loops.
  PipeFlowWind
        :: [PipeCore () Flow.Name]
        -> PipeFlow (C.AnTEC () Flow.Name)

  -- Wind loop# primops into tail recursive loops.
  PipeFlowToTetra
        :: [PipeCore () Salt.Name]
        -> PipeFlow (C.AnTEC () Flow.Name)


-- | Process a Core Flow module.
pipeFlow :: C.Module a Flow.Name
         -> PipeFlow a
         -> IO [Error]

pipeFlow !mm !pp
 = case pp of
        PipeFlowOutput !sink
         -> {-# SCC "PipeFlowOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeFlowHacks !(Canned f) !pipes
         -> {-# SCC "PipeFlowHacks" #-} 
            do  mm'     <- f mm
                pipeFlows mm' pipes

        PipeFlowPrep  !pipes
         -> {-# SCC "PipeFlowPrep"   #-}
            let 
                -- Eta-expand so all workers have explicit parameter names.
                mm_eta          = C.result $ Eta.etaModule Flow.profile
                                        (Eta.configZero { Eta.configExpand = True})
                                        mm

                -- Snip program to expose intermediate bindings.
                mm_snip         = Flatten.flatten 
                                $ Snip.snip 
                                        (Snip.configZero { Snip.configSnipLetBody = True })
                                        mm_eta

                -- The floater needs bindings to be fully and uniquely named,
                -- so we need to anonymize them before naming them.
                -- Otherwise the namifier might end up reusing names, which confuses the usage checker.
                mm_anon         = TMod.transformModX C.anonymizeX mm_snip

                namifierT       = C.makeNamifier Flow.freshT Env.empty
                namifierX       = C.makeNamifier Flow.freshX Env.empty
                mm_namified     = S.evalState (C.namify namifierT namifierX mm_anon) 0

                -- Float worker functions and initializers into their use sites, 
                -- leaving only flow operators at the top-level.
                mm_float        = Flow.forwardProcesses mm_namified

            in  pipeCores mm_float pipes

        PipeFlowRate  !pipes
         -> {-# SCC "PipeFlowRate"   #-}
            let 
                -- Eta-expand so all workers have explicit parameter names.
                mm_eta          = C.result $ Eta.etaModule Flow.profile
                                        (Eta.configZero { Eta.configExpand = True})
                                        mm

                -- Snip program to expose intermediate bindings.
                mm_snip         = Flatten.flatten 
                                $ Snip.snip 
                                        (Snip.configZero { Snip.configSnipLetBody = True })
                                        mm_eta

                -- The floater needs bindings to be fully named.
                namifierT       = C.makeNamifier Flow.freshT Env.empty
                namifierX       = C.makeNamifier Flow.freshX Env.empty
                mm_namified     = S.evalState (C.namify namifierT namifierX mm_snip) 0


                -- Forward all worker functions
                mm_float        = Flow.forwardProcesses mm_namified


                goRate
                 = case C.checkModule (C.configOfProfile Flow.profile) mm_float C.Recon of
                     (Left err, _)    
                      -> return [ErrorCoreTransform err]

                     (Right mm', _) 
                      -> let mm_stripped = C.reannotate (const ()) mm'
                             mm_flow     = fst $ Flow.seriesOfVectorModule mm_stripped
                           
                             -- Check again to synthesise types
                         in case C.checkModule (C.configOfProfile Flow.profile) mm_flow C.Recon of
                             (Left err, _ct)         
                              -> return [ErrorCoreTransform err]
                            
                             (Right mm_flow', _ct) 
                              -> let mm_reannot' = C.reannotate (const ()) mm_flow'
                                 in pipeCores mm_reannot' pipes
            in  goRate


        PipeFlowLower !config !pipes 
         -> {-# SCC "PipeFlowLower" #-}
            let mm_stripped     = C.reannotate (const ()) mm

            in  case Flow.lowerModule config mm_stripped of
                 Right mm'      -> pipeCores mm' pipes
                 Left  err      -> return [ErrorCoreTransform err]

        PipeFlowMelt !pipes
         -> {-# SCC "PipeFlowMelt" #-}
            let mm_stripped     = C.reannotate (const ()) mm
                (mm_melted, _info) = Flow.meltModule mm_stripped
            in  pipeCores mm_melted pipes

        PipeFlowWind !pipes
         -> {-# SCC "PipeFlowWind" #-}
            let mm_stripped     = C.reannotate (const ()) mm
                mm_wound        = Flow.windModule mm_stripped
            in  pipeCores mm_wound pipes

        PipeFlowToTetra !pipes
         -> {-# SCC "PipeFlowToTetra" #-}
            let 
                -- Apply any lambdas we can
                mm_beta         = C.result $ Beta.betaReduce Flow.profile
                                        (Beta.configZero { Beta.configBindRedexes = True})
                                        mm

                -- Eta-expand all the leftovers so they can be lifted
                mm_eta          = C.result $ Eta.etaModule   Flow.profile
                                        (Eta.configZero { Eta.configExpand = True})
                                        mm_beta


                -- Lift up any remaining lambdas
                mm_lift         = Lambdas.lambdasModule Flow.profile mm_eta

                -- The floater needs bindings to be fully named.
                namifierT       = C.makeNamifier Flow.freshT Env.empty
                namifierX       = C.makeNamifier Flow.freshX Env.empty
                mm_namified     = S.evalState (C.namify namifierT namifierX mm_lift) 0

                floatControl l
                 = case l of
                   C.LLet b _
                     | Just _ <- C.takeTFun $ C.typeOfBind b
                     -> Forward.FloatForce
                   _ -> Forward.FloatAllow

                -- Forward all functions
                mm_float        = C.result
                                $ Forward.forwardModule Flow.profile
                                    (Forward.Config floatControl True)
                                    $ C.reannotate (const ()) mm_namified

            in  case Flow.tetraOfFlowModule mm_float of
                 Left  err  -> return [ErrorFlowConvert err]
                 Right mm'  -> pipeCores mm' pipes 



-- | Process a Flow module with several different pipes.
pipeFlows :: (NFData a, Show a)
          => C.Module a Flow.Name -> [PipeFlow a] -> IO [Error]

pipeFlows !mm !pipes 
 = go [] pipes
 where  go !errs []   
         = return errs

        go !errs (pipe : rest)
         = do   !err     <- pipeFlow mm pipe
                go (errs ++ err) rest


