
module DDC.Core.Flow.Transform.Schedule.Nest
        ( -- * Insertion into a loop nest
          insertContext
        , insertStarts
        , insertBody
        , insertEnds

          -- * Rate predicates
        , nestContainsRate
        , nestContainsGuardedRate)
where
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Exp
import Data.Monoid


-------------------------------------------------------------------------------
-- | Insert a skeleton context into a nest.
--    The new context doesn't contain any statements, it just provides
--    the infrastructure to execute statements at the new rate.
--
--   TODO: what are the possible relationships between contexts?
--         Write examples that have NestGuards inside NestSegment
--         and vice versa.
--
insertContext :: Nest -> Context -> Maybe Nest

-- Context already exists, don't bother.
insertContext nest            context@ContextRate{}
 | nestContainsRate nest (contextRate context)
 = Just nest

-- Loop context at top level.
insertContext  NestEmpty      context@ContextRate{}
 = Just $ nestOfContext context


-- Drop Selector Context ------------------------
-- Selector context goes at this level in the loop nest.
insertContext nest@NestLoop{} context@ContextSelect{}
 | nestRate nest == contextOuterRate context
 , Just starts  <- startsForContext context
 = Just $ nest 
        { nestInner = nestInner nest <> nestOfContext context 
        , nestStart = nestStart nest ++ starts }

-- Selector context need to be inserted deeper in the nest.
insertContext nest@NestLoop{} context@ContextSelect{}
 | nestContainsRate nest (contextOuterRate context)
 , Just inner'  <- insertContext (nestInner nest) context
 , Just starts  <- startsForContext context
 = Just $ nest 
        { nestInner = inner' 
        , nestStart = nestStart nest ++ starts }

-- Selector context inserted inside an existing selector context.
insertContext nest@NestGuard{}   context@ContextSelect{}
 | nestInnerRate nest == contextOuterRate context
 = Just $ nest { nestInner = nestInner nest <> nestOfContext context }


-- Drop Segment Context -------------------------
-- Selector context goes at this level in the loop nest.
insertContext nest@NestLoop{} context@ContextSegment{}
 | nestRate nest == contextOuterRate context
 , Just starts  <- startsForContext context
 = Just $ nest
        { nestInner = nestInner nest <> nestOfContext context
        , nestStart = nestStart nest ++ starts }

-- TODO: do we allow segment contexts inside guards, and vice versa?

insertContext _nest _context
 = Nothing


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

insertStarts _ _ _
 = Nothing


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


insertBody nest@NestGuard{} context@(ContextRate tRate) body'
 | tRate == nestInnerRate nest
 = Just $ nest { nestBody = nestBody nest ++ body' }

 | Just inner'  <- insertBody (nestInner nest) context body'
 = Just $ nest { nestInner = inner' }


insertBody nest@NestSegment{} context@(ContextRate tRate) body'
 | tRate == nestInnerRate nest
 = Just $ nest { nestBody = nestBody nest ++ body' }

 | Just inner' <- insertBody (nestInner nest) context body'
 = Just $ nest { nestInner = inner' }


insertBody (NestList (n:ns)) context body'
 | Just n'  <- insertBody n context body'
 = Just $ NestList (n':ns)

insertBody (NestList (n:ns)) context body'
 | Just (NestList ns') <- insertBody (NestList ns) context body'
 = Just $ NestList (n:ns')

insertBody (NestList []) _ _
 = Nothing
 
insertBody _ _ _
 = Nothing


-------------------------------------------------------------------------------
-- | Insert ending statements in the given context.
insertEnds :: Nest -> Context -> [StmtEnd] -> Maybe Nest

-- The ends are for this loop.
insertEnds nest@NestLoop{} (ContextRate tRate) ends'
 | tRate == nestRate nest
 = Just $ nest { nestEnd = nestEnd nest ++ ends' }

-- The ends are for some inner context contained by this loop,
-- so we can still drop them here.
insertEnds nest@NestLoop{} (ContextRate tRate) ends'
 | nestContainsRate nest tRate
 = Just $ nest { nestEnd = nestEnd nest ++ ends' }
 
insertEnds _ _ _
 = Nothing


-- Rate Predicates ------------------------------------------------------------
-- | Check whether the top-level of this nest contains the given rate.
--   It might be in a nested context.
nestContainsRate :: Nest -> TypeF -> Bool
nestContainsRate nest tRate
 = case nest of
        NestEmpty       
         -> False

        NestList ns     
         -> any (flip nestContainsRate tRate) ns

        NestLoop{}
         ->  nestRate nest == tRate
          || nestContainsRate (nestInner nest) tRate

        NestGuard{}
         ->  nestInnerRate nest == tRate
          || nestContainsRate (nestInner nest) tRate

        NestSegment{}
         ->  nestInnerRate nest == tRate
          || nestContainsRate (nestInner nest) tRate


-- | Check whether the given rate is the inner rate of some 
--  `NestGuard` constructor.
nestContainsGuardedRate :: Nest -> TypeF -> Bool
nestContainsGuardedRate nest tRate
 = case nest of
        NestEmpty
         -> False

        NestList ns
         -> any (flip nestContainsRate tRate) ns

        NestLoop{}
         -> nestContainsGuardedRate (nestInner nest) tRate

        NestGuard{}
         -> nestInnerRate nest == tRate
         || nestContainsGuardedRate (nestInner nest) tRate

        NestSegment{}
         -> nestContainsGuardedRate (nestInner nest) tRate


-- Skeleton nests -------------------------------------------------------------
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
          , nestResult          = xUnit }

        ContextSelect{}
         -> NestGuard
          { nestOuterRate       = contextOuterRate context
          , nestInnerRate       = contextInnerRate context
          , nestFlags           = contextFlags     context
          , nestBody            = [] 
          , nestInner           = NestEmpty }

        ContextSegment{}
         -> NestSegment
          { nestOuterRate       = contextOuterRate context
          , nestInnerRate       = contextInnerRate context
          , nestLength          = contextLens      context
          , nestBody            = []
          , nestInner           = NestEmpty }


-- | For selector and segment contexts, make statements that initialize a 
--   counter for how many times the context has been entered.
startsForContext :: Context -> Maybe [StmtStart]
startsForContext context
 = case context of
        ContextSelect{}
         -> let TVar (UName nK) = contextInnerRate context
                nCounter        = NameVarMod nK "count"
            in  Just [ StartAcc 
                        { startAccName = nCounter
                        , startAccType = tNat
                        , startAccExp  = xNat 0 }]

        ContextSegment{}
         -> let TVar (UName nK) = contextInnerRate context
                nCounter        = NameVarMod nK "count"
            in  Just [ StartAcc 
                        { startAccName = nCounter
                        , startAccType = tNat
                        , startAccExp  = xNat 0 }]

        _  -> Nothing


