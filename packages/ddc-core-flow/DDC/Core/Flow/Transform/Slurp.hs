module DDC.Core.Flow.Transform.Slurp
        ( slurpProcesses
        , slurpOperator
        , isSeriesOperator
        , isVectorOperator)
where
import DDC.Core.Flow.Transform.Slurp.Operator
import DDC.Core.Flow.Transform.Slurp.Error
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Transform.Deannotate
import DDC.Core.Module
import qualified DDC.Type.Env           as Env
import DDC.Type.Env                     (TypeEnv)
import Data.List


-- | Slurp stream processes from the top level of a module.
slurpProcesses :: Module () Name -> Either Error [Process]
slurpProcesses mm
 = slurpProcessesX (deannotate (const Nothing) $ moduleBody mm)


-- | Slurp stream processes from a module body.
--   A module consists of some let-bindings wrapping a unit data constructor.
slurpProcessesX :: Exp () Name   -> Either Error [Process]
slurpProcessesX xx
 = case xx of
        -- Slurp processes definitions from the let-bindings.
        XLet lts x'
          -> do ps1     <- slurpProcessesLts lts 
                ps2     <- slurpProcessesX x'
                return  $ ps1 ++ ps2

        -- Ignore the unit data constructor at the end of the module.
        _
         | xx == xUnit  -> Right []
         | otherwise    -> Left $ ErrorBadProcess xx


-- | Slurp stream processes from the top-level let expressions.
slurpProcessesLts :: Lets () Name -> Either Error [Process]
slurpProcessesLts (LRec binds)
 = sequence [slurpProcessLet b x | (b, x) <- binds]

slurpProcessesLts (LLet b x)
 = sequence [slurpProcessLet b x]

slurpProcessesLts _
 = return []


-------------------------------------------------------------------------------
-- | Slurp stream operators from a top-level binding.
slurpProcessLet 
        :: Bind Name            -- ^ Binder for the whole process.
        -> Exp () Name          -- ^ Expression of body.
        -> Either Error Process

slurpProcessLet (BName n t) xx

 -- We assume that all type params come before the value params.
 | (snd $ takeTFunAllArgResult t) == tProcess
 , Just (fbs, xBody)    <- takeXLamFlags xx
 = let  
        -- Split binders into type and value binders.
        (fbts, fbvs)    = partition fst fbs

        -- Type binders.
        bts             = map snd fbts
        tsRate          = filter (\b -> typeOfBind b == kRate) bts

        -- Create contexts for all the parameter rate variables.
        ctxParam        = map (ContextRate . TVar . UName)
                        $ map (\(BName nRate _) -> nRate)
                        $ tsRate

        -- Value binders.
        bvs             = map snd fbvs

        -- Slurp the body of the process.
   in do
        (ctxLocal, ops) 
                <- slurpProcessX Env.empty xBody

        return  $ Process
                { processName          = n
                , processParamTypes    = bts
                , processParamValues   = bvs

                -- Note that the parameter contexts needs to come first
                -- so they are scheduled before the local contexts, which
                -- are inside 
                , processContexts      = ctxParam ++ ctxLocal

                , processOperators     = ops }

slurpProcessLet _ xx
 = Left (ErrorBadProcess xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest. 
-- 
--   The process type environment records what process bindings are in scope,
--   so that we can check that the overall process is well formed. 
--   This environment only needs to contain locally defined process variables,
--   not the global environment for the whole module.
--
slurpProcessX 
        :: TypeEnv Name         -- ^ Process type environment.
        -> ExpF                 -- ^ A sequence of non-recursive let-bindings.
        -> Either Error
                ( [Context]     --   Nested contexts created by this process.
                , [Operator])   --   Series operators in this binding.

slurpProcessX tenv xx
 | XLet (LLet b x) xMore        <- xx
 = do   
        -- Slurp operators from the binding.
        (ctxHere, opsHere)      <- slurpBindingX tenv b x

        -- If this binding defined a process then add it do the environment.
        let tenv'
                | typeOfBind b == tProcess = Env.extend b tenv
                | otherwise                = tenv

        -- Slurp the rest of the process using the new environment.
        (ctxMore, opsMore)      <- slurpProcessX tenv' xMore

        return  ( ctxHere ++ ctxMore
                , opsHere ++ opsMore)

 -- Only handle very simple cases with one alt for now.                 -- TODO: why do we accept
 -- 'Invert' the case and create a let binding for each binder.         ---    case in the middle of
 -- We can safely duplicate xScrut since it's in ANF.                   --     a process??
 | XCase xScrut [AAlt (PData dc bs) x]  <- xx
 , bs'  <- takeSubstBoundsOfBinds bs
 , length bs == length bs'
 , lets <- zipWith
              (\b b' -> LLet b
                (XCase xScrut
                 [AAlt (PData dc bs)
                       (XVar b')])) bs bs'
 = slurpProcessX tenv (xLets lets x)

-- Slurp a process ending.
slurpProcessX tenv xx
 -- The process ends with a variable that has Process# type.
 | XVar u       <- xx
 , Just t       <- Env.lookup u tenv
 , t == tProcess
 = return ([], [])                

 -- The process ends by joining two existing processes.
 -- We assume that the overall expression is well typed.
 | Just (NameOpSeries OpSeriesJoin, [_, _])     
                <- takeXPrimApps xx
 = return ([], [])

 -- Process finishes with some expression that doesn't look like it 
 -- actually defines a value of type Process#.
 | otherwise
 = Left (ErrorBadProcess xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from a let-binding.
slurpBindingX 
        :: TypeEnv Name         -- ^ Process type environment.
        -> BindF                -- ^ Binder to assign result to.
        -> ExpF                 -- ^ Right of the binding.
        -> Either 
                Error
                ( [Context]     --   Nested contexts created by this binding.
                , [Operator])   --   Series operators in this binding.

-- Decend into more let bindings.
-- We get these when entering into a nested context.
slurpBindingX tenv b1 xx
 | XLet (LLet b2 x2) xMore      <- xx
 = do   
        -- Slurp operators from the binding.
        (ctxHere, opsHere)      <- slurpBindingX tenv b2 x2

        -- If this binding defined a process then add it to the environement.
        let tenv'
                | typeOfBind b2 == tProcess = Env.extend b2 tenv
                | otherwise                 = tenv

        -- Slurp the rest of the process using the new environment.
        (ctxMore, opsMore)      <- slurpBindingX tenv' b1 xMore

        return  ( ctxHere ++ ctxMore
                , opsHere ++ opsMore)

-- Slurp a mkSel1#
-- This creates a nested selector context.
slurpBindingX tenv b 
 (   takeXPrimApps 
  -> Just ( NameOpSeries (OpSeriesMkSel 1)
          , [ XType tK1
            , XVar uFlags
            , XLAM (BName nR kR) (XLam bSel xBody)]))
 | kR == kRate
 = do
        (ctxInner, osInner)
                <- slurpBindingX tenv b xBody

        -- Add an intermediate edge from the flags variable to its use. 
        -- This is needed for the case when the flags series is one of the
        -- parameters to the process, because the intermediate OpId forces 
        -- the scheduler to add the  flags_elem = next [k] flags_series 
        -- statement.
        let UName nFlags = uFlags
        let nFlagsUse   = NameVarMod nFlags "use"
        let uFlagsUse   = UName nFlagsUse
        let bFlagsUse   = BName nFlagsUse (tSeries tK1 tBool)

        let opId        = OpId
                        { opResultSeries        = bFlagsUse
                        , opInputRate           = tK1
                        , opInputSeries         = uFlags 
                        , opElemType            = tBool }

        let context     = ContextSelect
                        { contextOuterRate      = tK1
                        , contextInnerRate      = TVar (UName nR)
                        , contextFlags          = uFlagsUse
                        , contextSelector       = bSel }

        return (context : ctxInner, opId : osInner)

-- Slurp an operator that doesn't introduce a new context.
slurpBindingX _ b xx
 | Just op      <- slurpOperator b xx
 = return ([], [op])

-- Slurp a process ending.
slurpBindingX tenv _ xx
 -- The process ends with a variable that has Process# type.
 | XVar u       <- xx
 , Just t       <- Env.lookup u tenv
 , t == tProcess
 = return ([], [])                

 -- The process ends by joining two existing processes.
 -- We assume that the overall expression is well typed.
 | Just (NameOpSeries OpSeriesJoin, [_, _])     
                <- takeXPrimApps xx
 = return ([], [])

 -- Process finishes with some expression that doesn't look like it 
 -- actually defines a value of type Process#.
 | otherwise
 = Left (ErrorBadOperator xx)

