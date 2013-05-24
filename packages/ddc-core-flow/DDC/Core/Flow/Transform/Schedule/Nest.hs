
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

-- | Insert a skeleton context into a nest.
--    The new context doesn't contain any statements, it just provides
--    the infrastructure to execute statements at the new rate.
insertContext :: Nest -> Context -> Maybe Nest

-- Loop context at top level.
insertContext  NestEmpty      context@ContextRate{}
 = Just $ nestOfContext context

-- Selector context inside loop context.
insertContext nest@NestLoop{} context@ContextSelect{}
 | nestRate nest == contextOuterRate context
 = Just $ nest { nestInner = nestInner nest <> nestOfContext context }

-- Nested selector context inside selector context.
insertContext nest@NestIf{}   context@ContextSelect{}
 | nestInnerRate nest == contextOuterRate context
 = Just $ nest { nestInner = nestInner nest <> nestOfContext context }

-- Selector context needs to be inserted deeper in this nest.
insertContext nest@NestLoop{} context@ContextSelect{}
 | nestContainsRate nest (contextOuterRate context)
 , Just inner'  <- insertContext (nestInner nest) context
 = Just $ nest { nestInner = inner' }

insertContext _nest _context
 = Nothing


-- | Yield a skeleton nest for a given context.
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
          , nestBody            = [] 
          , nestInner           = NestEmpty }


-- | Check whether the top-level of this nest contains the given rate.
--   It might be in a nested context.
nestContainsRate :: Nest -> Type Name -> Bool
nestContainsRate nest tRate
 = case nest of
        NestEmpty       
         -> False

        NestList ns     
         -> any (flip nestContainsRate tRate) ns

        NestLoop{}
         ->  nestRate nest == tRate
          || nestContainsRate (nestInner nest) tRate

        NestIf{}
         ->  nestInnerRate nest == tRate
          || nestContainsRate (nestInner nest) tRate


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: Nest -> Context -> [StmtStart] -> Maybe Nest

-- The starts are for this loop.
insertStarts nest@NestLoop{} (ContextRate tRate) starts'
 | tRate == nestRate nest
 = Just $ nest { nestStart = nestStart nest ++ starts' }

-- The starts are for some inner context contained by this loop, 
-- so we can still drop them here.
insertStarts nest@NestLoop{} (ContextRate tRate) starts'
 | nestContainsRate nest tRate
 = Just $ nest { nestStart = nestStart nest ++ starts' }

insertStarts nest context _
 = error $ show (nest, context)


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

insertBody nest@NestIf{}   context@(ContextRate tRate) body'
 | tRate == nestInnerRate nest
 = Just $ nest { nestBody = nestBody nest ++ body' }

 | Just inner'  <- insertBody (nestInner nest) context body'
 = Just $ nest { nestInner = inner' }
 
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

