
module DDC.Core.Flow.Transform.Schedule.Nest
        ( insertContext
        , insertStarts
        , insertBody
        , insertEnds)
where
import DDC.Core.Flow.Procedure
import DDC.Core.Compounds
import DDC.Core.Flow.Prim
import DDC.Type.Exp
import Data.Monoid


-------------------------------------------------------------------------------
insertContext :: Nest -> Context -> Maybe Nest
insertContext  NestEmpty      context@ContextRate{}
 = Just $ nestOfContext context

insertContext nest@NestLoop{} context@ContextSelect{}
 | nestRate nest == contextOuterRate context
 = Just $ nest { nestInner = nestInner nest <> nestOfContext context }

insertContext _ _
 = Nothing


nestOfContext :: Context -> Nest
nestOfContext context
 = case context of
        ContextRate tRate
         -> NestLoop
          { nestRate            = tRate
          , nestStart           = []
          , nestBody            = []
          , nestInner           = NestEmpty
          , nestEnd             = []
          , nestResult          = xUnit () }

        ContextSelect{}
         -> NestIf
          { nestOuterRate       = contextOuterRate context
          , nestInnerRate       = contextInnerRate context
          , nestFlags           = contextFlags     context
          , nestBody            = [] }


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: Nest -> Context -> [StmtStart] -> Maybe Nest

-- The starts are for this loop.
insertStarts nest@NestLoop{} (ContextRate tRate) starts'
 | tRate == nestRate nest
 = Just $ nest { nestStart = nestStart nest ++ starts' }

-- The starts are for some inner context dominated by this loop, 
-- so we can still drop them here.
insertStarts nest@NestLoop{} (ContextRate tRate) starts'
 | nestDominatesRate nest tRate
 = Just $ nest { nestStart = nestStart nest ++ starts' }

insertStarts nest context _
 = error $ show (nest, context)


-- | Check whether the top-level of this nest dominates the given rate.
--   The nest is at a rate at least as large as the given rate.
nestDominatesRate :: Nest -> Type Name -> Bool
nestDominatesRate nest tRate
 = case nest of
        NestEmpty       
         -> False

        NestList ns     
         -> any (flip nestDominatesRate tRate) ns

        NestLoop{}
         | nestRate nest == tRate       
         -> True

         | otherwise                    
         -> nestDominatesRate (nestInner nest) tRate

        NestIf{}
         -> nestInnerRate nest == tRate



-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertBody :: Nest -> Context -> [StmtBody] -> Maybe Nest

insertBody nest@NestLoop{} context@(ContextRate tRate) body'
 -- If the desired context is the same as the loop then we can drop
 -- the statements right here.
 | tRate == nestRate nest
 = Just $ nest { nestBody = nestBody nest ++ body' }

 -- Try and insert them in an inner context.
 | Just inner'  <- insertBody (nestInner nest) context body'
 = Just $ nest { nestInner = inner' }

insertBody nest@NestIf{}   (ContextRate tRate) body'
 | tRate == nestInnerRate nest
 = Just $ nest { nestBody = nestBody nest ++ body' }


insertBody _ _ _
 = Nothing

-------------------------------------------------------------------------------
-- | Insert ending statements in the given context.
insertEnds :: Nest -> Context -> [StmtEnd] -> Maybe Nest
insertEnds nest@NestLoop{} (ContextRate tRate) ends'
 | tRate == nestRate nest
 = Just $ nest { nestEnd = nestEnd nest ++ ends' }

insertEnds _ _ _
 = Nothing

