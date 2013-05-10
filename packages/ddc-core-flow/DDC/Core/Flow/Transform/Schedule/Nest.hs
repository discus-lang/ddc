
module DDC.Core.Flow.Transform.Schedule.Nest
        ( insertStarts
        , insertBody
        , insertEnds)
where
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds


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
