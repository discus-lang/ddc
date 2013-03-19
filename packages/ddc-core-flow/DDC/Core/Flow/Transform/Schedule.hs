
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
                , loopNested            = []
                , loopEnd               = [] 
                , loopResult            = xUnit () }


-- | Schedule a single stream operator into a loop nest.
scheduleOperator :: Loop -> Operator -> Loop
scheduleOperator loop op
 | OpFold{}                     <- op
 , BName n@(NameVar strName) t  <- opResult op
 = let  
        nAcc    = NameVar $ strName ++ "_acc"

        loop1   = insertStarts loop   ContextTop
                        [ StartAcc nAcc t (opZero op) ]

        loop2   = insertBody   loop1 (ContextRate (opRate op))
                        [ BodyAcc nAcc t
                                (opStream op)
                                (opWorkerBody op) ]

        loop3   = insertEnds   loop2  ContextTop
                        [ EndAcc   n t nAcc ]

   in   loop3

 -- TODO: we're assuming the only plain statement is the result expression.
 | OpBase x           <- op
 = loop { loopResult  = x }

 | otherwise
 = error "scheduleOperator: can't schedule"
        

-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: Loop -> Context -> [StmtStart] -> Loop
insertStarts (Loop c starts body nested end result) _c' starts'       -- TODO: put into correct context.
 = Loop c (starts ++ starts') body nested end result


-- | Insert starting statements in the given context.
insertBody :: Loop -> Context -> [StmtBody] -> Loop
insertBody (Loop c starts body nested end result) _c' body'       -- TODO: put into correct context.
 = Loop c starts (body ++ body') nested end result

-- | Insert ending statements in the given context.
insertEnds :: Loop -> Context -> [StmtEnd] -> Loop
insertEnds (Loop c starts body nested end result) _c' ends'           -- TODO: put into correct context.
 = Loop c starts body nested (end ++ ends') result
