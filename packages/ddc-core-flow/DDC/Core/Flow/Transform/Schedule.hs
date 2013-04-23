
module DDC.Core.Flow.Transform.Schedule
        (scheduleProcess)
where
import DDC.Core.Flow.Exp.Procedure
import DDC.Core.Flow.Exp.Process
import DDC.Core.Flow.Prim
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
                , processOperators      = ops 
                , processResult         = xResult})
  =     (Procedure
                { procedureName         = name
                , procedureType         = ty
                , procedureParamTypes   = psType
                , procedureParamValues  = psValue
                , procedureNest         = foldl' scheduleOperator [] ops 
                , procedureResult       = xResult })


-- | Schedule a single stream operator into a loop nest.
scheduleOperator :: [Loop] -> Operator -> [Loop]
scheduleOperator nest op
 | OpFold{}                     <- op
 , BName n@(NameVar strName) _  <- opResult op
 = let  
        nAcc    = NameVar $ strName ++ "_acc"
        tElem   = opTypeStream op

        context = Context (opRate op)
        nest1   = insertStarts nest  context
                   [ StartAcc nAcc tElem (opZero op) ]

        nest2   = insertBody   nest1 context
                   [ BodyAccRead  nAcc tElem (opWorkerParamAcc op)
                   , BodyAccWrite nAcc tElem 
                                (opStream op) 
                                (opWorkerParamElem op)
                                (opWorkerBody op) ]

        nest3   = insertEnds   nest2 context
                        [ EndAcc   n tElem nAcc ]
   in   nest3

 -- TODO: we're assuming the only plain statement is the result expression.
 | OpBase x           <- op
 , loopTop : loops    <- nest
 = loopTop { loopResult  = x } : loops

 | otherwise
 = error "scheduleOperator: can't schedule"
        

-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
--   TODO: proper context.
insertStarts :: [Loop] -> Context -> [StmtStart] -> [Loop]

insertStarts [] c' starts' 
 = insertStarts
    [Loop c' [] [] [] [] (xUnit ())] c' starts'

insertStarts (loop : loops) c' starts' 
 | Loop c starts body nested end result        <- loop
 , c == c'
 = Loop c (starts ++ starts') body nested end result : loops

 | otherwise
 = loop : insertStarts loops c' starts'


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
--   TODO: proper context
insertBody :: [Loop] -> Context -> [StmtBody] -> [Loop]

insertBody [] c' body'
 = insertBody 
    [Loop c' [] [] [] [] (xUnit ())] c' body'

insertBody  (loop : loops) c' body' 
 | Loop c starts body nested end result         <- loop
 , c == c'
 = Loop c starts (body ++ body') nested end result : loops

 | otherwise
 = loop : insertBody loops c' body'


-------------------------------------------------------------------------------
-- | Insert ending statements in the given context.
insertEnds :: [Loop] -> Context -> [StmtEnd] -> [Loop] 

insertEnds [] c' ends' 
 = insertEnds 
    [Loop c' [] [] [] [] (xUnit ())] c' ends'

insertEnds (loop : loops) c' ends' 
 | Loop c starts body nested end result       <- loop
 , c == c'
 = Loop c starts body nested (end ++ ends') result : loops

 | otherwise
 = loop : insertEnds loops c' ends'




