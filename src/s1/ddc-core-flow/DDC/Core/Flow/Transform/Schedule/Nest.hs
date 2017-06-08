
module DDC.Core.Flow.Transform.Schedule.Nest
        ( -- * Insertion into a loop nest
          scheduleContext
        )
where
import DDC.Core.Flow.Procedure
import DDC.Core.Flow.Process
import DDC.Core.Flow.Exp
import DDC.Core.Flow.Transform.Schedule.Error
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
import DDC.Core.Flow.Context

import Control.Arrow
import qualified Data.Map as Map

scheduleContext
    :: (Type Name -> Context  -> Either Error (Type Name))
    -> (FillMap -> Operator -> Either Error ([StmtStart], [StmtBody], [StmtEnd]))
    -> Context
    -> Either Error Nest

scheduleContext frate fop topctx
 = do   fills <- maybe (Left ErrorMultipleFills) Right
               $ pathsOfFills topctx

        let (starts', ends')  = allocAndTrunc fills

        (starts, nest, ends) <- go topctx
         
        return $ insertStarts (starts' ++ starts)
               $ insertEnds   (ends'   ++ ends)
               $ nest
 where
  fop' op
   = do fills <- maybe (Left ErrorMultipleFills) Right
               $ pathsOfFills topctx
        fop fills op


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

             let nest = NestGuard
                      { nestOuterRate  = rateOuter
                      , nestInnerRate  = rateInner
                      , nestFlags      = contextFlags     ctx
                      , nestBody  = bodies
                      , nestInner = i2 }

             return ( s1 ++ s2
                    , nest
                    , e1 ++ e2)


      ContextSegment{}
       -> do (s1,bodies,e1) <- ops   ctx
             (s2,i2,    e2) <- inner ctx

             rateOuter      <- frate (contextOuterRate ctx) ctx
             rateInner      <- frate (contextInnerRate ctx) ctx


             let nest = NestSegment
                      { nestOuterRate  = rateOuter
                      , nestInnerRate  = rateInner
                      , nestLength     = contextLens      ctx
                      , nestBody  = bodies
                      , nestInner = i2 }

             return ( s1 ++ s2
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
   = do outs <- mapM fop' (contextOps ctx)
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


allocAndTrunc :: FillMap -> ([StmtStart], [StmtEnd])
allocAndTrunc fills
 = concat *** concat
 $ unzip
 $ map go 
 $ Map.toList fills
 where
  go (k,(f,t))
   | isSimple f || isNone f
   = ([], [])
   | otherwise
   = let k' = getAccForPath fills f
         kk = maybe k id k'
         co = NameVarMod kk "count"

         s  | k == kk
            = [StartAcc
              { startAccName = co
              , startAccType = tNat
              , startAccExp  = xNat 0 } ]
            | otherwise
            = []

         e  = [EndVecTrunc
                k t
                (UName co) ]

     in  (s, e)


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

