

module DDC.Core.Flow.Lower
        ( Config        (..)
        , defaultConfigScalar
        , defaultConfigVector
        , Method        (..)
        , lowerModule)
where
import DDC.Core.Flow.Transform.Slurp
import DDC.Core.Flow.Transform.Schedule
import DDC.Core.Flow.Transform.Extract
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Exp
import DDC.Core.Module

import DDC.Core.Transform.Annotate

import qualified DDC.Core.Simplifier                    as C
import qualified DDC.Core.Simplifier.Recipe             as C
import qualified DDC.Core.Transform.Namify              as C
import qualified DDC.Core.Transform.Snip                as Snip
import qualified DDC.Type.Env                           as Env
import qualified Control.Monad.State.Strict             as S
import qualified Data.Monoid                            as M


-- | Configuration for the lower transform.
data Config
        = Config
        { configMethod          :: Method }
        deriving (Eq, Show)


-- | What lowering method to use.
data Method
        -- | Produce sequential scalar code with nested loops.
        = MethodScalar

        -- | Try to produce sequential vector code,
        --   falling back to scalar code if this is not possible.
        | MethodVector
        { methodLifting         :: Lifting }
        deriving (Eq, Show)


defaultConfigScalar :: Config
defaultConfigScalar
        = Config
        { configMethod  = MethodScalar }


defaultConfigVector :: Config
defaultConfigVector
        = Config
        { configMethod  = MethodVector (Lifting 4)}


-- Lower ----------------------------------------------------------------------
lowerModule :: Config -> ModuleF -> ModuleF
lowerModule config mm
 = let  
        -- Slurp out series processes.
        processes       = slurpProcesses mm

        -- Schedule processeses into procedures.
        lets            = map (lowerProcess config) processes

        -- Stash all the processes into a module.
        mm_lowered      = mm
                        { moduleBody    = annotate ()
                                        $ XLet (LRec lets) xUnit }

        -- Clean up extracted code
        mm_clean        = cleanModule mm_lowered
   in   mm_clean


-- | Lower a single series process into fused code.
lowerProcess :: Config -> Process -> (BindF, ExpF)
lowerProcess config process
 | MethodScalar         <- configMethod config
 = let  
        -- Schedule process into scalar code.
        Right proc              = scheduleScalar process

        -- Extract code for the kernel
        (bProc, xProc)          = extractProcedure proc

   in   (bProc, xProc)


 | MethodVector lifting <- configMethod config
 = let  
        -- Create the vector version of the kernel.
        Right procVec           = scheduleKernel lifting process

        -- Extract code for the kernel
        (bProcVec, xProcVec)    = extractProcedure procVec

   in   (bProcVec, xProcVec)

 | otherwise
 = error "ddc-core-flow.lowerProcess: invalid lowering method"


-- Clean ----------------------------------------------------------------------
-- | Do some beta-reductions to ensure that arguments to worker functions
--   are inlined, then normalize nested applications. 
--   When snipping, leave lambda abstractions in place so the worker functions
--   applied to our loop combinators aren't moved.
cleanModule :: ModuleF -> ModuleF
cleanModule mm
 = let
        clean           
         =    C.Trans (C.Namify (C.makeNamifier freshT)
                                (C.makeNamifier freshX))
         M.<> C.Trans C.Forward
         M.<> C.beta
         M.<> C.Trans (C.Snip (Snip.configZero { Snip.configPreserveLambdas = True }))
         M.<> C.Trans C.Flatten

        mm_cleaned      
         = S.evalState
                (C.applySimplifier profile Env.empty Env.empty
                        (C.Fix 4 clean) mm)
                0
   in   mm_cleaned

