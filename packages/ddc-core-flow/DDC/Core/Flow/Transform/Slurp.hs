module DDC.Core.Flow.Transform.Slurp
        (slurpProcesses)
where
import DDC.Core.Flow.Transform.Slurp.Alloc
import DDC.Core.Flow.Transform.Slurp.Operator
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Exp
import DDC.Core.Transform.Deannotate
import DDC.Core.Module
import Data.Maybe
import Data.List


-- | Slurp stream processes from the top level of a module.
slurpProcesses :: Module () Name -> [Process]
slurpProcesses mm
 = slurpProcessesX (deannotate (const Nothing) $ moduleBody mm)


-- | Slurp stream processes from a module body.
slurpProcessesX :: Exp () Name   -> [Process]
slurpProcessesX xx
 = case xx of
        XLet lts x'
          -> slurpProcessesLts lts ++ slurpProcessesX x'

        _ -> []


-- | Slurp stream processes from the top-level let expressions.
slurpProcessesLts :: Lets () Name -> [Process]
slurpProcessesLts (LRec binds)
 = catMaybes [slurpProcessLet b x | (b, x) <- binds]

slurpProcessesLts (LLet b x)
 = catMaybes [slurpProcessLet b x]

slurpProcessesLts _
 = []


-------------------------------------------------------------------------------
-- | Slurp stream operators from a top-level binding.
slurpProcessLet :: Bind Name -> Exp () Name -> Maybe Process
slurpProcessLet (BName n tProcess) xx

 -- We assume that all type params come before the value params.
 | Just (fbs, xBody)    <- takeXLamFlags xx
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
        (ctxLocal, ops, ltss, xResult)  
                        = slurpProcessX xBody

        -- Decide what rates to use when allocating vectors.
        ops_alloc       = patchAllocRates ops

        -- Determine the type of the result of the process.
        tResult         = snd $ takeTFunAllArgResult tProcess

   in   Just    $ Process
                { processName          = n
                , processParamTypes    = bts
                , processParamValues   = bvs

                -- Note that the parameter contexts needs to come first
                -- so they are scheduled before the local contexts, which
                -- are inside 
                , processContexts      = ctxParam ++ ctxLocal

                , processOperators     = ops_alloc
                , processStmts         = ltss
                , processResultType    = tResult
                , processResult        = xResult }

slurpProcessLet _ _
 = Nothing


-------------------------------------------------------------------------------
-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest.
slurpProcessX 
        :: ExpF                 -- A sequence of non-recursive let-bindings.
        -> ( [Context]          -- Nested contexts created by this process.
           , [Operator]         -- Series operators in this binding.
           , [LetsF]            -- Baseband statements that don't process series.
           , ExpF)              -- Final value of process.

slurpProcessX xx
 | XLet (LLet b x) xMore                <- xx
 , (ctxHere, opsHere, ltsHere)          <- slurpBindingX b x
 , (ctxMore, opsMore, ltsMore, xResult) <- slurpProcessX xMore
 = ( ctxHere ++ ctxMore
   , opsHere ++ opsMore
   , ltsHere ++ ltsMore
   , xResult)

 -- Only handle very simple cases with one alt for now.
 -- 'Invert' the case and create a let binding for each binder.
 -- We can safely duplicate xScrut since it's in ANF.
 | XCase xScrut [AAlt (PData dc bs) x]  <- xx
 , bs'  <- takeSubstBoundsOfBinds bs
 , length bs == length bs'
 , lets <- zipWith
              (\b b' -> LLet b
                (XCase xScrut
                 [AAlt (PData dc bs)
                       (XVar b')])) bs bs'
 = slurpProcessX (xLets lets x)

 | otherwise
 = ([], [], [], xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from a let-binding.
slurpBindingX 
        :: BindF                -- Binder to assign result to.
        -> ExpF                 -- Right of the binding.
        -> ( [Context]          -- Nested contexts created by this binding.
           , [Operator]         -- Series operators in this binding.
           , [LetsF])           -- Baseband statements that don't process series.

-- Decend into more let bindings.
-- We get these when entering into a nested context.
slurpBindingX b1 xx
 | XLet (LLet b2 x2) xMore      <- xx
 , (ctxHere, opsHere, ltsHere)  <- slurpBindingX b2 x2
 , (ctxMore, opsMore, ltsMore)  <- slurpBindingX b1 xMore
 = ( ctxHere ++ ctxMore
   , opsHere ++ opsMore
   , ltsHere ++ ltsMore)

-- Slurp a mkSel1#
-- This creates a nested selector context.
slurpBindingX b 
 (   takeXPrimApps 
  -> Just ( NameOpFlow (OpFlowMkSel 1)
          , [ XType tK1, XType _tA
            , XVar uFlags
            , XLAM (BName nR kR) (XLam bSel xBody)]))
 | kR == kRate
 = let  
        (ctxInner, osInner, ltsInner)
                = slurpBindingX b xBody

        -- Add an intermediate edge from the flags variable to its use. 
        -- This is needed for the case when the flags series is one of the
        -- parameters to the process, because the intermediate OpId forces 
        -- the scheduler to add the  flags_elem = next [k] flags_series 
        -- statement.
        UName nFlags    = uFlags
        nFlagsUse       = NameVarMod nFlags "use"
        uFlagsUse       = UName nFlagsUse
        bFlagsUse       = BName nFlagsUse (tSeries tK1 tBool)

        opId    = OpId
                { opResultSeries        = bFlagsUse
                , opInputRate           = tK1
                , opInputSeries         = uFlags 
                , opElemType            = tBool }

        context = ContextSelect
                { contextOuterRate      = tK1
                , contextInnerRate      = TVar (UName nR)
                , contextFlags          = uFlagsUse
                , contextSelector       = bSel }

   in   (context : ctxInner, opId : osInner, ltsInner)

-- | Slurp an operator that doesn't introduce a new context.
slurpBindingX b x
 = case slurpOperator b x of

        -- This binding is a flow operator.        
        Just op -> ([], [op], [])

        -- This is some base-band statement that doesn't 
        -- work on a flow operator.
        _       -> ([], [], [LLet b x])

