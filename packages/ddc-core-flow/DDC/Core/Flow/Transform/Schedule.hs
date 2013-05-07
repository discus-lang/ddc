
module DDC.Core.Flow.Transform.Schedule
        (scheduleProcess)
where
import DDC.Core.Flow.Exp.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Base.Pretty
import Data.List


-- | Create loops from a list of operators.
--   TODO: allow the operators to be at different rates, 
--         and create multiple loops as needed.
scheduleProcess :: Process -> Procedure
scheduleProcess 
        (Process 
                { processName           = name
                , processParamTypes     = psType
                , processParamValues    = psValue
                , processOperators      = ops 
                , processStmts          = stmts
                , processResultType     = tResult
                , processResult         = xResult})
  =     (Procedure
                { procedureName         = name
                , procedureParamTypes   = psType
                , procedureParamValues  = psValue
                , procedureNest         = foldl' scheduleOperator [] ops 
                , procedureStmts        = stmts
                , procedureResultType   = tResult
                , procedureResult       = xResult })


-- | Schedule a single stream operator into a loop nest.
scheduleOperator :: [Loop] -> Operator -> [Loop]
scheduleOperator nest0 op
 | OpMap{}                      <- op
 , BName n@(NameVar strName) _  <- opResult op
 = let  
        nest1   = insertBody   nest1 context
                  [ ]                   !!!!!!! not finished

 | OpFold{}                     <- op
 , BName n@(NameVar strName) _  <- opResult op
 = let  
        nAcc    = NameVar $ strName ++ "_acc"
        tElem   = opTypeElem op

        context = Context (opRate op)
        nest1   = insertStarts nest0 context
                   [ StartAcc nAcc tElem (opZero op) ]

        nest2   = insertBody   nest1 context
                   [ BodyAccRead  nAcc tElem 
                        (opWorkerParamAcc op)
                   , BodyAccWrite nAcc tElem 
                        (opInput op) 
                        (opWorkerParamElem op)
                        (opWorkerBody op) ]

        nest3   = insertEnds   nest2 context
                   [ EndAcc   n tElem nAcc ]
   in   nest3

 | otherwise
 = error $ renderIndent 
 $ vcat [ text "repa-plugin: can't schedule operator"
        , ppr op ]


        

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




