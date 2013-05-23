{-# LANGUAGE ViewPatterns #-}
module DDC.Core.Flow.Transform.Slurp
        (slurpProcesses)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Context
import DDC.Core.Flow.Process
import DDC.Core.Flow.Compounds
import Data.Maybe
import Data.List


-- | Slurp stream processes from the top level of a module.
slurpProcesses :: Module () Name -> [Process]
slurpProcesses mm
 = slurpProcessesX (moduleBody mm)


-- | Slurp stream processes from a module body.
slurpProcessesX :: Exp () Name   -> [Process]
slurpProcessesX xx
 = case xx of
        XLet _ lts x'
          -> slurpProcessesLts lts ++ slurpProcessesX x'

        _ -> []


-- | Slurp stream processes from the top-level let expressions.
slurpProcessesLts :: Lets () Name -> [Process]
slurpProcessesLts (LRec binds)
 = catMaybes [slurpProcessLet b x | (b, x) <- binds]

slurpProcessesLts (LLet _ b x)
 = catMaybes [slurpProcessLet b x]

slurpProcessesLts _
 = []


-------------------------------------------------------------------------------
-- | Slurp stream operators from a top-level binding.
slurpProcessLet :: Bind Name -> Exp () Name -> Maybe Process
slurpProcessLet (BName n tProcess) xx

 -- TODO: check that all the type params come before the value params.
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

                , processOperators     = ops
                , processStmts         = ltss
                , processResultType    = tResult
                , processResult        = xResult }

slurpProcessLet _ _
 = Nothing


-------------------------------------------------------------------------------
-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest.
slurpProcessX 
        :: Exp () Name          -- A sequence of non-recursive let-bindings.
        -> ( [Context]          -- Nested contexts created by this process.
           , [Operator]         -- Series operators in this binding.
           , [Lets () Name]     -- Baseband statements that don't process series.
           , Exp () Name)       -- Final value of process.

slurpProcessX xx
 | XLet _ (LLet _ b x) xMore            <- xx
 , (ctxHere, opsHere, ltsHere)          <- slurpBindingX b x
 , (ctxMore, opsMore, ltsMore, xResult) <- slurpProcessX xMore
 = ( ctxHere ++ ctxMore
   , opsHere ++ opsMore
   , ltsHere ++ ltsMore
   , xResult)

 | otherwise
 = ([], [], [], xx)


-------------------------------------------------------------------------------
-- | Slurp stream operators from a let-binding.
slurpBindingX 
        :: Bind Name            -- Binder to assign result to.
        -> Exp () Name          -- Right of the binding.
        -> ( [Context]          -- Nested contexts created by this binding.
           , [Operator]         -- Series operators in this binding.
           , [Lets () Name])    -- Baseband statements that don't process series.

-- Decend into more let bindings.
-- We get these when entering into a nested context.
slurpBindingX b1 xx
 | XLet _ (LLet _ b2 x2) xMore            <- xx
 , (ctxHere, opsHere, ltsHere)          <- slurpBindingX b2 x2
 , (ctxMore, opsMore, ltsMore)          <- slurpBindingX b1 xMore
 = ( ctxHere ++ ctxMore
   , opsHere ++ opsMore
   , ltsHere ++ ltsMore)

-- Slurp a mkSel1#
-- This creates a nested selector context.
slurpBindingX b 
 (   takeXPrimApps 
  -> Just ( NameOpFlow (OpFlowMkSel 1)
          , [ XType tK1, XType _tA
            , XVar _ uFlags
            , XLAM _ (BName nR kR) (XLam _ bSel xBody)]))
 | kR == kRate
 = let  
        (ctxInner, osInner, ltsInner)
                = slurpBindingX b xBody

        context = ContextSelect
                { contextOuterRate      = tK1
                , contextInnerRate      = TVar (UName nR)
                , contextFlags          = uFlags
                , contextSelector       = bSel }

   in   (context : ctxInner, osInner, ltsInner)

-- | Slurp an operator that doesn't introduce a new context.
slurpBindingX b x
 = case slurpOperator b x of

        -- This binding is a flow operator.        
        Just op -> ([], [op], [])

        -- This is some base-band statement that doesn't 
        -- work on a flow operator.
        _       -> ([], [], [LLet LetStrict b x])

