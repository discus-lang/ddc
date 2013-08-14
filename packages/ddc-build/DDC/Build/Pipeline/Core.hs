{-# LANGUAGE GADTs #-}
module DDC.Build.Pipeline.Core
        ( PipeCore (..)
        , pipeCore
        , pipeCores

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
import DDC.Data.Canned
import DDC.Llvm.Pretty                                  ()

import qualified DDC.Core.Flow                          as Flow
import qualified DDC.Core.Flow.Profile                  as Flow
import qualified DDC.Core.Flow.Transform.Slurp          as Flow
import qualified DDC.Core.Flow.Transform.Melt           as Flow
import qualified DDC.Core.Flow.Transform.Wind           as Flow

import qualified DDC.Core.Lite                          as Lite

import qualified DDC.Core.Salt.Platform                 as Salt
import qualified DDC.Core.Salt.Runtime                  as Salt
import qualified DDC.Core.Salt                          as Salt

import qualified DDC.Core.Transform.Reannotate          as C
import qualified DDC.Core.Transform.Deannotate          as C
import qualified DDC.Core.Transform.Namify              as C
import qualified DDC.Core.Transform.Forward             as Forward
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Core.Transform.Flatten             as Flatten
import qualified DDC.Core.Transform.Eta                 as Eta
import qualified DDC.Core.Simplifier                    as C

import qualified DDC.Core.Fragment                      as C
import qualified DDC.Core.Check                         as C
import qualified DDC.Core.Module                        as C
import qualified DDC.Core.Exp                           as C

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
        :: !Sink 
        -> PipeCore a n

  -- Type check a module.
  PipeCoreCheck      
        :: !(Fragment n err)
        -> ![PipeCore (C.AnTEC a n) n]
        -> PipeCore a n

  -- Type check a module, discarding previous per-node type annotations.
  PipeCoreReCheck
        :: (Show a, NFData a)
        => !(Fragment n err)
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
        :: !(Fragment n err)
        -> !s
        -> !(Simplifier s a n)
        -> ![PipeCore () n] 
        -> PipeCore a n

  -- Treat a module as belonging to the Core Lite fragment from now on.
  PipeCoreAsLite
        :: ![PipeLite]
        -> PipeCore (C.AnTEC () Lite.Name) Lite.Name

  -- Treat a module as beloning to the Core Flow fragment from now on.
  PipeCoreAsFlow 
        :: Pretty a
        => ![PipeFlow a]
        -> PipeCore a Flow.Name

  -- Treat a module as beloning to the Core Salt fragment from now on.
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

        PipeCoreOutput !sink
         -> {-# SCC "PipeCoreOutput" #-}
            pipeSink (renderIndent $ ppr mm) sink

        PipeCoreCheck !fragment !pipes
         -> {-# SCC "PipeCoreCheck" #-}
            let profile         = fragmentProfile fragment

                goCheck mm1
                 = case C.checkModule (C.configOfProfile profile) mm1 of
                        Left err   -> return [ErrorLint err]
                        Right mm2  -> goComplies mm2

                goComplies mm1
                 = case C.complies profile mm1 of
                        Just err   -> return [ErrorLint err]
                        Nothing    -> pipeCores mm1 pipes

             in goCheck mm

        PipeCoreReCheck !fragment !pipes
         -> {-# SCC "PipeCoreReCheck" #-}
            pipeCore (C.reannotate C.annotTail mm)
         $  PipeCoreCheck fragment pipes

        PipeCoreReannotate f !pipes
         -> {-# SCC "PipeCoreStrip" #-}
            let mm' = (C.reannotate f mm)
            in  pipeCores mm' pipes

        PipeCoreSimplify !fragment !nameZero !simpl !pipes
         -> {-# SCC "PipeCoreSimplify" #-}
            let profile         = fragmentProfile fragment
                primKindEnv     = C.profilePrimKinds      profile
                primTypeEnv     = C.profilePrimTypes      profile

                !mm'            = (flip S.evalState nameZero
                                   $ applySimplifier profile primKindEnv primTypeEnv simpl mm)

                !mm2            = C.reannotate (const ()) mm'

                -- NOTE: It is helpful to deepseq here so that we release 
                --       references to the unsimplified version of the code.
                --       Because we've just applied reannotate, we also
                --       release type annotations on the expression tree.
            in  mm2 `deepseq` pipeCores mm2 pipes

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

                -- The floater needs bindings to be fully named.
                namifierT       = C.makeNamifier Flow.freshT Env.empty
                namifierX       = C.makeNamifier Flow.freshX Env.empty
                mm_namified     = S.evalState (C.namify namifierT namifierX mm_snip) 0

                -- Float worker functions and initializers into their use sites, 
                -- leaving only flow operators at the top-level.
                isFloatable lts
                 = case lts of
                    C.LLet (C.BName _ _) x
                      |  Flow.isFlowOperator (C.deannotate (const Nothing) x)
                      -> Forward.FloatDeny
                    _ -> Forward.FloatForce

                mm_float        = C.result $ Forward.forwardModule Flow.profile 
                                        (Forward.Config isFloatable False)
                                        mm_namified

            in  pipeCores mm_float pipes

        PipeFlowLower !config !pipes 
         -> {-# SCC "PipeFlowLower" #-}
            let mm_stripped     = C.reannotate (const ()) mm
                mm_lowered      = Flow.lowerModule config mm_stripped
             in pipeCores mm_lowered pipes

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


