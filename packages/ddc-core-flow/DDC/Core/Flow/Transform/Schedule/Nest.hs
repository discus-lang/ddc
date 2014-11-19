
module DDC.Core.Flow.Transform.Schedule.Nest
        ( -- * Insertion into a loop nest
          scheduleContext

          -- * Rate predicates
        , nestContainsRate
        , nestContainsGuardedRate)
where
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Schedule.Error
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds

scheduleContext
    :: (Type Name -> Context  -> Either Error (Type Name))
    -> (Operator -> Either Error ([StmtStart], [StmtBody], [StmtEnd]))
    -> Context
    -> Either Error Nest

scheduleContext frate fop topctx
 = do   (starts, nest, ends) <- go topctx
        return $ insertStarts starts (insertEnds ends nest)
 where
  go ctx
   = case ctx of
      ContextRate{}
       -> do (s1,bodies,e1) <- ops   ctx
             (s2,i2,    e2) <- inner ctx
             rate'          <- frate (contextRate ctx) ctx

             let nest = NestLoop
                      { nestRate  = rate'
                      , nestStart = []
                      , nestBody  = bodies
                      , nestInner = i2
                      , nestEnd   = [] }

             return ( s1 ++ s2
                    , nest
                    , e1 ++ e2)

      ContextSelect{}
       -> do (s1,bodies,e1) <- ops   ctx
             (s2,i2, e2) <- inner ctx

             rateOuter      <- frate (contextOuterRate ctx) ctx
             rateInner      <- frate (contextInnerRate ctx) ctx

             let TVar (UName n) = contextInnerRate ctx
             let sacc = StartAcc
                      { startAccName = NameVarMod n "count"
                      , startAccType = tNat
                      , startAccExp  = xNat 0 }

             let nest = NestGuard
                      { nestOuterRate  = rateOuter
                      , nestInnerRate  = rateInner
                      , nestFlags      = contextFlags     ctx
                      , nestBody  = bodies
                      , nestInner = i2 }

             return ( s1 ++ [sacc] ++ s2
                    , nest
                    , e1 ++ e2)


      ContextSegment{}
       -> do (s1,bodies,e1) <- ops   ctx
             (s2,i2,    e2) <- inner ctx

             rateOuter      <- frate (contextOuterRate ctx) ctx
             rateInner      <- frate (contextInnerRate ctx) ctx


             let TVar (UName n) = contextInnerRate ctx
             let sacc = StartAcc
                      { startAccName = NameVarMod n "count"
                      , startAccType = tNat
                      , startAccExp  = xNat 0 }

             let nest = NestSegment
                      { nestOuterRate  = rateOuter
                      , nestInnerRate  = rateInner
                      , nestLength     = contextLens      ctx
                      , nestBody  = bodies
                      , nestInner = i2 }

             return ( s1 ++ [sacc] ++ s2
                    , nest
                    , e1 ++ e2)

      ContextAppend{}
       -> do (s1,i1,e1)     <- go (contextInner1 ctx)
             (s2,i2,e2)     <- go (contextInner2 ctx)

             let nest = NestList
                      [ i1, i2 ]

             return ( s1 ++ s2
                    , nest
                    , e1 ++ e2)



  ops ctx
   = do outs <- mapM fop (contextOps ctx)
        let (ss,bs,es) = unzip3 outs
        return (concat ss, concat bs, concat es)

  inner ctx
   = do outs <- mapM go  (contextInner ctx)
        let (ss,ins,es) = unzip3 outs
        return (concat ss, listNest ins, concat es)

  listNest []
   = NestEmpty
  listNest [n]
   = n
  listNest ns
   = NestList ns

-------------------------------------------------------------------------------
-- | Insert starting statements in the given context.
insertStarts :: [StmtStart] -> Nest -> Nest
insertStarts starts' nest
 = case nest of
    NestList (n:ns)
     -> NestList (insertStarts starts' n : ns) 
    NestLoop{}
     -> nest { nestStart = nestStart nest ++ starts' }
    _
     -> nest

-------------------------------------------------------------------------------
-- | Insert ends statements in the given context.
insertEnds :: [StmtEnd] -> Nest -> Nest
insertEnds ends' nest
 = case nest of
    NestList ns
     | (r:rs) <- reverse ns
     -> NestList (reverse rs ++ [insertEnds ends' r])
    NestLoop{}
     -> nest { nestEnd = nestEnd nest ++ ends' }
    _
     -> nest

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


