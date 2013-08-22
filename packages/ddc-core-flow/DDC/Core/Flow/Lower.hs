

module DDC.Core.Flow.Lower
        ( Config        (..)
        , defaultConfigScalar
        , defaultConfigKernel
        , defaultConfigVector
        , Method        (..)
        , lowerModule)
where
import DDC.Core.Flow.Transform.Slurp
import DDC.Core.Flow.Transform.Schedule
import DDC.Core.Flow.Transform.Schedule.Base
import DDC.Core.Flow.Transform.Extract
import DDC.Core.Flow.Process
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
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
import Control.Monad


-- | Configuration for the lower transform.
data Config
        = Config
        { configMethod          :: Method }
        deriving (Eq, Show)


-- | What lowering method to use.
data Method
        -- | Produce sequential scalar code with nested loops.
        = MethodScalar

        -- | Produce vector kernel code that only processes an even multiple
        --   of the vector width.
        | MethodKernel
        { methodLifting         :: Lifting }

        -- | Try to produce sequential vector code,
        --   falling back to scalar code if this is not possible.
        | MethodVector
        { methodLifting         :: Lifting }
        deriving (Eq, Show)


defaultConfigScalar :: Config
defaultConfigScalar
        = Config
        { configMethod  = MethodScalar }


defaultConfigKernel :: Config
defaultConfigKernel
        = Config
        { configMethod  = MethodKernel (Lifting 4)}


defaultConfigVector :: Config
defaultConfigVector
        = Config
        { configMethod  = MethodVector (Lifting 4)}


-- Lower ----------------------------------------------------------------------
lowerModule :: Config -> ModuleF -> Either Fail ModuleF
lowerModule config mm
 = do 
        -- Slurp out series processes.
        let processes   = slurpProcesses mm

        -- Schedule processeses into procedures.
        lets            <- mapM (lowerProcess config) processes

        -- Stash all the processes into a module.
        let mm_lowered  = mm
                        { moduleBody    = annotate ()
                                        $ XLet (LRec lets) xUnit }

        -- Clean up extracted code
        let mm_clean        = cleanModule mm_lowered
        return mm_clean


-- | Lower a single series process into fused code.
lowerProcess :: Config -> Process -> Either Fail (BindF, ExpF)
lowerProcess config process
 | MethodScalar         <- configMethod config
 = do  
        -- Schedule process into scalar code.
        let Right proc              = scheduleScalar process

        -- Extract code for the kernel
        let (bProc, xProc)          = extractProcedure proc

        return (bProc, xProc)


 | MethodVector lifting <- configMethod config

 , BName nRN tRN : _  <- processParamValues process
 , isRateNatType tRN

 = do   let c           = liftingFactor lifting


        -- Get the primary rate variable.
        let bK : _      = processParamTypes process
        let Just uK     = takeSubstBoundOfBind bK
        let tK          = TVar uK

        -- The RateNat witness
        let xRN         = XVar (UName nRN)

        -----------------------------------------
        -- Create the vector version of the kernel.
        --  Vector code processes several elements per loop iteration.
        procVec         <- scheduleKernel lifting process
        let (_, xProcVec) = extractProcedure procVec
        
        
        let bxsDownSeries       
                = [ ( bS
                    , ( BName (NameVarMod n "down")
                              (tSeries (tDown c tK) tE)
                      , xDown c tK tE (XVar (UIx 0)) xS))
                  | bS@(BName n tS)  <- processParamValues process
                  , let Just tE = elemTypeOfSeriesType tS
                  , let Just uS = takeSubstBoundOfBind bS
                  , let xS      = XVar uS
                  , isSeriesType tS ]

        -- Get a value arg to give to the vector procedure.
        let getDownValArg b
                | Just (b', _)  <- lookup b bxsDownSeries
                = liftM XVar $ takeSubstBoundOfBind b'

                | otherwise
                = liftM XVar $ takeSubstBoundOfBind b

        let Just xsVecValArgs    
                = sequence 
                $ map getDownValArg (processParamValues process)

        let bRateDown
                = BAnon (tRateNat (tDown c (TVar uK)))

        let xProcVec'       
                = XLam bRateDown
                $ xLets [LLet b x | (_, (b, x)) <- bxsDownSeries]
                $ xApps (XApp xProcVec (XType (TVar uK)))
                $ xsVecValArgs


        -----------------------------------------
        -- Create tail version.
        --  Scalar code processes the final elements of the loop.
        procTail        <- scheduleScalar process
        let (_, xProcTail) = extractProcedure procTail


        let bxsTailSeries
                = [ ( bS
                    , ( BName (NameVarMod n "tail")
                              (tSeries (tTail c tK) tE)
                      , xTail c tK tE (XVar (UIx 0)) xS))
                  | bS@(BName n tS)    <- processParamValues process
                  , let Just tE = elemTypeOfSeriesType tS
                  , let Just uS = takeSubstBoundOfBind bS
                  , let xS      = XVar uS
                  , isSeriesType tS ]

        -- Get a value arg to give to the scalar procedure.
        let getTailValArg b
                | Just (b', _)  <- lookup b bxsTailSeries
                = liftM XVar $ takeSubstBoundOfBind b'

                | otherwise
                = liftM XVar $ takeSubstBoundOfBind b

        let Just xsTailValArgs
                = sequence 
                $ map getTailValArg (procedureParamValues procTail)

        let bRateTail
                = BAnon (tRateNat (tTail c (TVar uK)))

        let xProcTail'
                = XLam bRateTail
                $ xLets [LLet b x | (_, (b, x)) <- bxsTailSeries]
                $ xApps (XApp xProcTail (XType (tTail c (TVar uK))))
                $ xsTailValArgs

        ------------------------------------------
        -- Stich the vector and scalar versions together.
        let xProc
                = foldr XLAM 
                       (foldr XLam xBody (processParamValues process))
                       (processParamTypes process)

            xBody
                = XLet (LLet   (BNone tUnit) 
                               (xSplit c (TVar uK) xRN xProcVec' xProcTail'))
                       xUnit
                
        -- Reconstruct a binder for the whole procedure / process.
        let bProc
                = BName (processName process)
                        (typeOfProcess process)

        return (bProc, xProc)

 | MethodKernel lifting <- configMethod config
 = do
        -- Schedule process into 
        proc            <- scheduleKernel lifting process

        -- Extract code for the kernel
        let (bProc, xProc)  = extractProcedure proc

        return (bProc, xProc)

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

