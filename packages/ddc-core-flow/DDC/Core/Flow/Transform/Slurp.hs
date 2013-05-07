
module DDC.Core.Flow.Transform.Slurp
        (slurpProcesses)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Process
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


-- | Slurp stream operators from a top-level binding.
slurpProcessLet :: Bind Name -> Exp () Name -> Maybe Process
slurpProcessLet (BName n _) xx

 -- TODO: check that all the type params come before the value params.
 | Just (fbs, xBody)      <- takeXLamFlags xx
 , (fbts, fbvs)           <- partition fst fbs
 , (ops,  ltss, xResult)  <- slurpProcessX xBody
 , o : _                  <- ops  -- TODO: requires at least one operator
 = let  Just tElem      = elemTypeOfOperator o          
                                -- TODO: doesn't handle OpBase, or multiple
                                --       streams of different types.
   in   Just $ Process
         { processName          = n
         , processType          = tElem
         , processParamTypes    = map snd fbts
         , processParamValues   = map snd fbvs
         , processOperators     = ops
         , processStmts         = ltss
         , processResult        = xResult }

slurpProcessLet _ _
 = Nothing


-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest.
slurpProcessX :: Exp () Name -> ([Operator], [Lets () Name], Exp () Name)
slurpProcessX xx
 | XLet _ lts@(LLet _ b x) xMore    <- xx
 , (opsMore, ltss, xBase)           <- slurpProcessX xMore
 = case slurpOperator b x of

        -- This binding is a flow operator.        
        Just op -> (op : opsMore, ltss,       xBase)

        -- This is some base-band statement that doesn't 
        -- work on a flow operator.
        _       -> (     opsMore, lts : ltss, xBase)

 | otherwise
 = ([], [], xx)



