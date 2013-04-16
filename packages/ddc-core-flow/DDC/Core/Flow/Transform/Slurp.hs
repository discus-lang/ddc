
module DDC.Core.Flow.Transform.Slurp
        (slurpProcesses)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import DDC.Core.Flow.Name
import DDC.Core.Flow.Exp.Process
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

slurpProcessesLts _
 = []


-- | Slurp stream operators from a top-level binding.
slurpProcessLet :: Bind Name -> Exp () Name -> Maybe Process
slurpProcessLet (BName n t) xx

 -- TODO: check that all the type params come before the value params.
 | Just (fbs, xBody)    <- takeXLamFlags xx
 , (fbts, fbvs)         <- partition fst fbs
 , (ops,  xResult)      <- slurpProcessX xBody
 = Just $ Process
        { processName          = n
        , processType          = t
        , processParamTypes    = map snd fbts
        , processParamValues   = map snd fbvs
        , processOperators     = ops
        , processResult        = xResult }

slurpProcessLet _ _
 = Nothing


-- | Slurp stream operators from the body of a function and add them to 
--   the provided loop nest.
slurpProcessX :: Exp () Name -> ([Operator], Exp () Name)
slurpProcessX xx
 | XLet _ (LLet _ b x) xMore    <- xx
 , (opsMore, xBase)             <- slurpProcessX xMore
 = case slurpOperator b x of
        Just op -> (op : opsMore, xBase)
        _       -> (     opsMore, xBase)

 | otherwise
 = ([], xx)



