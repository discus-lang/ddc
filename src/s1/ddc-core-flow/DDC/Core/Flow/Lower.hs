

module DDC.Core.Flow.Lower
        ( Config        (..)
        , defaultConfigScalar
        , defaultConfigKernel
        , defaultConfigVector
        , Method        (..)
        , Lifting       (..)
        , lowerModule)
where
import DDC.Core.Transform.TransformUpX                  ()
import DDC.Core.Flow.Transform.Slurp
import DDC.Core.Flow.Transform.Schedule
import DDC.Core.Flow.Transform.Schedule.Base
import DDC.Core.Flow.Transform.Extract
import DDC.Core.Flow.Transform.TransformUpX
import DDC.Core.Flow.Transform.Annotate
import DDC.Core.Flow.Transform.Deannotate
import DDC.Core.Flow.Process
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Profile
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import DDC.Core.Module


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


-- | Config for producing code with just scalar operations.
defaultConfigScalar :: Config
defaultConfigScalar
        = Config
        { configMethod  = MethodScalar }


-- | Config for producing code with vector operations, 
--   where the loops just handle a size of data which is an even multiple
--   of the vector width.
defaultConfigKernel :: Config
defaultConfigKernel
        = Config
        { configMethod  = MethodKernel (Lifting 8)}


-- | Config for producing code with vector operations, 
--   where the loops handle arbitrary data sizes, of any number of elements.
defaultConfigVector :: Config
defaultConfigVector
        = Config
        { configMethod  = MethodVector (Lifting 8)}


-- Lower ----------------------------------------------------------------------
-- | Take a module that contains some well formed series processes defined
--   at top-level, and lower them into procedures. 
lowerModule :: Config -> ModuleF -> Either Error ModuleF
lowerModule config mm
 = case slurpProcesses mm of
    -- Can't slurp a process definition from one of the top level series
    -- processes. 
    Left  err   
     -> Left (ErrorSlurpError err)

    -- We've got a process definition for all of then.
    Right procs
     -> do      
        -- Find names of all process bindings
        let procname (Left  p) = [processName p]
            procname (Right _) = []

            procnames   = concatMap procname procs

        -- Schedule the processeses into procedures.
        lets            <- mapM (lowerEither config procnames) procs

        -- Wrap all the procedures into a new module.
        let mm_lowered  = mm
                        { moduleBody    = annotate ()
                                        $ XLet (LRec lets) xUnit }

        -- Clean up extracted code
        let mm_clean        = cleanModule mm_lowered
        return mm_clean


-- | Look at slurped result, and if it's a process lower it, otherwise remove any runProcess# inside expressions
lowerEither  :: Config -> [Name] -> (Either Process (Bind Name, Exp () Name)) -> Either Error (BindF, ExpF)
lowerEither  config _ (Left process)
 = lowerProcess config process

lowerEither _config _procnames (Right (b,xx))
 = let xx' = deannotate (const Nothing)
           $ transformSimpleUpX' replaceRunProc
           $ annotate () xx
   in  return (b, xx')
 where

  -- Replace all calls to runProcess# with runProcessUnit#
  replaceRunProc (XVar (UPrim (NameOpSeries OpSeriesRunProcess) _))
   = Just
   $ XVar
   $ UPrim (NameOpSeries OpSeriesRunProcessUnit)
           (typeOpSeries OpSeriesRunProcessUnit)
  -- Also replace any Process# types with Units
  replaceRunProc (XType t)
   = Just
   $ XType (replaceProcTy t)
  replaceRunProc (XLet (LLet bind x) e)
   = Just
   $ XLet (LLet (replaceProcTyB bind) x) e
  replaceRunProc (XLet (LRec bxs) e)
   | (bs,xs) <- unzip bxs
   , bs'     <- map replaceProcTyB bs
   = Just
   $ XLet (LRec (zip bs' xs)) e

  replaceRunProc _
   = Nothing

  replaceProcTyB (BName n t) = BName n $ replaceProcTy t
  replaceProcTyB (BAnon   t) = BAnon   $ replaceProcTy t
  replaceProcTyB (BNone   t) = BNone   $ replaceProcTy t

  -- Replace Process# a b with Unit
  replaceProcTy tt
   = case tt of
      TVar{} -> tt
      TCon{} -> tt

      TAbs bind tt'
       -> TAbs bind (replaceProcTy tt')

      TApp l r
       | Just (NameTyConFlow TyConFlowProcess, [_,_]) <- takePrimTyConApps tt
       -> tUnit
       | otherwise
       -> TApp (replaceProcTy l) (replaceProcTy r)

      TForall bind tt'
       -> TForall bind (replaceProcTy tt')

      TSum ts
       -> TSum ts
 

-- | Lower a single series process into fused code.
lowerProcess :: Config -> Process -> Either Error (BindF, ExpF)
lowerProcess config process
 
 -- Scalar lowering ------------------------------
 | MethodScalar         <- configMethod config
 = do  
        -- Schedule process into scalar code.
        proc                 <- scheduleScalar process

        -- Extract code for the kernel
        let (bProc, xProc)    = extractProcedure proc

        return (bProc, xProc)


 -- Vector lowering -----------------------------
 -- To use the vector method, 
 --  the type of the source function needs to have a quantifier for
 --  the rate variable (k), as well as a (RateNat k) witness.
 --
 | MethodVector lifting <- configMethod config
 , [nRN]  <- [ nRN | (flag, BName nRN tRN) <- processParamFlags process
                   , not flag
                   , isRateNatType tRN ]
 , tK <- processLoopRate process
 = do   let c           = liftingFactor lifting

        -- The RateNat witness
        let xRN         = XVar (UName nRN)

        let tProc       = processProcType process
        let _tLoopRate   = processLoopRate process

        -----------------------------------------
        -- Create the vector version of the kernel.
        --  Vector code processes several elements per loop iteration.
        procVec         <- scheduleKernel lifting process
        let (_, xProcVec) = extractProcedure procVec
        
        
        let bxsDownSeries       
                = [ ( bS
                    , ( BName (NameVarMod n "down")
                              (tSeries tProc (tDown c tK) tE)
                      , xDown c tProc tK tE (XVar (UIx 0)) xS))
                  | (flag, bS@(BName n tS)) <- processParamFlags process
                  , not flag
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
                $ map getDownValArg 
                $ map snd
                $ filter (not.fst)
                $ processParamFlags process

        let bRateDown
                = BAnon (tRateNat (tDown c tK))

        let xProcVec'       
                = XLam bRateDown
                $ xLets [LLet b x | (_, (b, x)) <- bxsDownSeries]
                $ xApps xProcVec
                $ [XType tProc, XType tK] ++ xsVecValArgs


        -----------------------------------------
        -- Create tail version.
        --  Scalar code processes the final elements of the loop.
        procTail        <- scheduleScalar process
        let (bProcTail, xProcTail) = extractProcedure procTail

        -- Window the input series to select the tails.
        let bxsTailSeries
                = [ ( bS, ( BName (NameVarMod n "tail") (tSeries tProc (tTail c tK) tE)
                          , xTail c tProc tK tE (XVar (UIx 0)) xS))
                  | (flag, bS@(BName n tS)) <- processParamFlags process
                  , not flag
                  , let Just tE = elemTypeOfSeriesType tS
                  , let Just uS = takeSubstBoundOfBind bS
                  , let xS      = XVar uS
                  , isSeriesType tS ]

        -- Window the output vectors to select the tails.
        let bxsTailVector
                = [ ( bV, ( BName (NameVarMod n "tail") (tVector tE)
                          , xTailVector c tK tE (XVar (UIx 0)) xV))
                  | (flag, bV@(BName n tV)) <- processParamFlags process
                  , not flag
                  , let Just tE = elemTypeOfVectorType tV
                  , let Just uV = takeSubstBoundOfBind bV
                  , let xV      = XVar uV
                  , isVectorType tV ]

        -- Get a value arg to give to the scalar procedure.
        let getTailValArg b
                | Just (b', _)  <- lookup b bxsTailSeries
                = liftM XVar $ takeSubstBoundOfBind b'

                | Just (b', _)  <- lookup b bxsTailVector
                = liftM XVar $ takeSubstBoundOfBind b'

                | otherwise
                = liftM XVar $ takeSubstBoundOfBind b

        let Just xsTailValArgs
                = sequence 
                $ map getTailValArg (map snd $ filter (not.fst) $ procedureParamFlags procTail)

        let bRateTail
                = BAnon (tRateNat (tTail c tK))

        let xProcTail'
                = XLam bRateTail
                $ xLets [LLet b x | (_, (b, x)) <- bxsTailSeries]
                $ xLets [LLet b x | (_, (b, x)) <- bxsTailVector]
                $ xApps xProcTail
                $ [XType tProc, XType (tTail c tK)] ++ xsTailValArgs

        ------------------------------------------
        -- Stich the vector and scalar versions together.
        let xProc
                = makeXLamFlags (processParamFlags process)
                                xBody

            xBody
                = XLet (LLet   (BNone tUnit) 
                               (xSplit c tK xRN xProcVec' xProcTail'))
                       xUnit
                
        -- Reconstruct a binder for the whole procedure / process.
        let bProc
                = BName (processName process)
                        (typeOfBind bProcTail)

        return (bProc, xProc)

 -- Kernel lowering -----------------------------
 | MethodKernel lifting <- configMethod config
 = do
        -- Schedule process into 
        proc            <- scheduleKernel lifting process

        -- Extract code for the kernel
        let (bProc, xProc)  = extractProcedure proc

        return (bProc, xProc)

 | otherwise
 = error $  "ddc-core-flow.lowerProcess: invalid lowering method"
         


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
         = C.result $ S.evalState
                (C.applySimplifier profile Env.empty Env.empty
                        (C.Fix 4 clean) mm)
                0
   in   mm_cleaned

