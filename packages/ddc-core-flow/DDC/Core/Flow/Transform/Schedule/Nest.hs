
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
 = insertStarts [newLoopForContext c'] c' starts'

insertStarts (loop : loops) c' starts' 
 | Loop c tRate starts body nested end result        <- loop
 , c == c'
 = Loop c tRate (starts ++ starts') body nested end result : loops

 | otherwise
 = loop : insertStarts loops c' starts'


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
--   TODO: proper context
insertBody :: [Loop] -> Context -> [StmtBody] -> [Loop]

insertBody [] c' body'
 = insertBody [newLoopForContext c'] c' body'

insertBody  (loop : loops) c' body' 
 | Loop c tRate starts body nested end result         <- loop
 , c == c'
 = Loop c tRate starts (body ++ body') nested end result : loops

 | otherwise
 = loop : insertBody loops c' body'


-------------------------------------------------------------------------------
-- | Insert ending statements in the given context.
insertEnds :: [Loop] -> Context -> [StmtEnd] -> [Loop] 

insertEnds [] c' ends' 
 = insertEnds [newLoopForContext c'] c' ends'

insertEnds (loop : loops) c' ends' 
 | Loop c tRate starts body nested end result       <- loop
 , c == c'
 = Loop c tRate starts body nested (end ++ ends') result : loops

 | otherwise
 = loop : insertEnds loops c' ends'


-------------------------------------------------------------------------------
newLoopForContext :: Context -> Loop
newLoopForContext c@(ContextRate tRate)
 = Loop c tRate [] [] [] [] (xUnit ())

newLoopForContext _
        = error "newLoopForContext: blerk"
