
module DDC.Core.Flow.Transform.Schedule
        (scheduleProcess)
where
import DDC.Core.Flow.Exp.Procedure
import DDC.Core.Flow.Exp.Process
import DDC.Core.Flow.Name
import DDC.Core.Compounds
import DDC.Core.Exp
import Data.List


-- | Create loops from a list of operators.
--   TODO: allow the operators to be at different rates, 
--         and create multiple loops as needed.
scheduleProcess :: Process -> Procedure
scheduleProcess 
        (Process 
                { processName           = name
                , processType           = ty
                , processParamTypes     = psType
                , processParamValues    = psValue
                , processOperators      = ops })
  =     (Procedure
                { procedureName         = name
                , procedureType         = ty
                , procedureParamTypes   = psType
                , procedureParamValues  = psValue
                , procedureLoop         = foldl' scheduleOperator loop0 ops })
  where
        loop0   = Loop
                { loopContext           = ContextTop
                , loopStart             = []
                , loopBody              = []
                , loopEnd               = [] 
                , loopResult            = xUnit () }


-- | Schedule a single stream operator into a loop nest.
scheduleOperator :: Loop -> Operator -> Loop
scheduleOperator loop op
 | OpFold{}                     <- op
 , BName n@(NameVar strName) t  <- opResult op
 = let  
        nAcc    = NameVar $ strName ++ "_acc"

        loop1   = insertStarts ContextTop loop
                        [ StartAcc nAcc t (opZero op) ]

        loop2   = insertEnds   ContextTop loop1
                        [ EndAcc   n t nAcc ]
   in   loop2

 -- TODO: we're assuming the only plain statement is the result expression.
 | OpBase x           <- op
 = loop { loopResult  = x }

 | otherwise
 = error "scheduleOperator: can't schedule"
        

-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: Context -> Loop -> [StmtStart] -> Loop
insertStarts _c' (Loop c starts body end result) starts'       -- TODO: put into correct context.
 = Loop c (starts ++ starts') body end result


-- | Insert ending statements in the given context.
insertEnds :: Context -> Loop -> [StmtEnd] -> Loop
insertEnds _c' (Loop c starts body end result) ends'           -- TODO: put into correct context.
 = Loop c starts body (end ++ ends') result
