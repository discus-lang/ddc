
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

insertContext _nest _context
 = Nothing


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: Nest -> TypeF -> [StmtStart] -> Maybe Nest
insertStarts nest tRate starts'
 = case nest of
        NestLoop{}
         -- Desired context is right here.
         |  tRate == nestRate nest
         -> Just $ nest { nestStart = nestStart nest ++ starts' }

         -- Desired context is deeper in the nest.
         -- The starting statements run before all interations of it.
         |  nestContainsRate nest tRate
         -> Just $ nest { nestStart = nestStart nest ++ starts' }

        _ -> Nothing


-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertBody :: Nest -> TypeF -> [StmtBody] -> Maybe Nest
insertBody nest tRate body'
 = case nest of
        NestLoop{}
         -- Desired context is right here.
         |  tRate == nestRate nest
         -> Just $ nest { nestBody = nestBody nest ++ body' }

         -- Desired context is deeper in the nest.
         |  Just inner' <- insertBody (nestInner nest) tRate body'
         -> Just $ nest { nestInner = inner' }

        NestGuard{}
         -- Desired context is right here.
         |  tRate == nestInnerRate nest
         -> Just $ nest { nestBody = nestBody nest ++ body' }

         -- Desired context is deeper in the nest.
         |  Just inner' <- insertBody (nestInner nest) tRate body'
         -> Just $ nest { nestInner = inner' }

        NestSegment{}
         -- Desired context is right here.
         |  tRate == nestInnerRate nest
         -> Just $ nest { nestBody = nestBody nest ++ body' }

         -- Desired context is deeper in the nest.
         |  Just inner' <- insertBody (nestInner nest) tRate body'
         -> Just $ nest { nestInner = inner' }

        NestList (n : ns)
         |  Just n'             <- insertBody n tRate body'
         -> Just $ NestList (n':ns)

         |  Just (NestList ns') <- insertBody (NestList ns) tRate body'
         -> Just $ NestList (n:ns')


        _ -> Nothing


-------------------------------------------------------------------------------
-- | Insert ending statements in the given context.
insertEnds :: Nest -> TypeF -> [StmtEnd] -> Maybe Nest
insertEnds nest tRate ends'
 = case nest of
        NestLoop{}
         -- Desired context is right here.
         |  tRate == nestRate nest
         -> Just $ nest { nestEnd = nestEnd nest ++ ends' }

         -- Desired context is deeper in the nest.
         -- The ending statements run before all iterations of it.
         |  nestContainsRate nest tRate
         -> Just $ nest { nestEnd = nestEnd nest ++ ends' }
 
        _ -> Nothing


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
          , nestEnd             = [] }

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

