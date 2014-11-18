
module DDC.Core.Flow.Transform.Slurp.Context
    ( insertContext )
where
import DDC.Core.Flow.Context
import DDC.Core.Flow.Transform.Slurp.Error
import DDC.Core.Flow.Prim
import DDC.Core.Compounds.Simple
import Control.Applicative


-- "embed" is to be pushed inside "into"
-- "embed" must contain no inner contexts.
-- "embed" also cannot be an append
insertContext
        :: Context
        -> Context
        -> Either Error Context
insertContext embed into
 = case into of
    ContextRate{}
     -> case embed of
         ContextRate{}
          | contextRate into == contextRate embed
          -> dropops
          | otherwise
          -> descend

         ContextSelect{}
          -> descendorpush (contextRate into == contextOuterRate embed)

         ContextSegment{}
          -> descendorpush (contextRate into == contextOuterRate embed)

         ContextAppend{}
          -> Left ErrorCannotMergeContext


    ContextSelect{}
     -> case embed of
         ContextRate{}
          | contextOuterRate into == contextRate embed
          -> dropops
          | otherwise
          -> descend

         ContextSelect{}
          | contextOuterRate into == contextOuterRate embed
          , contextInnerRate into == contextInnerRate embed
          , contextFlags     into == contextFlags     embed
          , contextSelector  into == contextSelector  embed
          -> dropops
          | otherwise
          -> descendorpush (contextInnerRate into == contextOuterRate embed)

         ContextSegment{}
          -> descendorpush (contextInnerRate into == contextOuterRate embed)

         ContextAppend{}
          -> Left ErrorCannotMergeContext


    ContextSegment{}
     -> case embed of
         ContextRate{}
          | contextOuterRate into == contextRate embed
          -> dropops
          | otherwise
          -> descend

         ContextSelect{}
          -> descendorpush (contextInnerRate into == contextOuterRate embed)

         ContextSegment{}
          | contextOuterRate into == contextOuterRate embed
          , contextInnerRate into == contextInnerRate embed
          , contextLens      into == contextLens      embed
          , contextSegd      into == contextSegd      embed
          -> dropops
          | otherwise
          -> descendorpush (contextInnerRate into == contextOuterRate embed)

         ContextAppend{}
          -> Left ErrorCannotMergeContext


    ContextAppend{}
     -> do      (ls, rs) <- splitContextIntoApps embed
                ls'      <- insertContext (contextInner1 into) ls
                rs'      <- insertContext (contextInner2 into) rs
                return $ into
                         { contextInner1 = ls'
                         , contextInner2 = rs' }

 where
  descend =
   case tryInserts embed (contextInner into) of
    Nothing -> Left ErrorCannotMergeContext
    Just cs -> return into { contextInner = cs }

  dropops =
   return
       into { contextOps   = contextOps   into ++ contextOps   embed }

  pushinner =
   return
       into { contextInner = contextInner into ++ [embed] }

  descendorpush p =
   case descend of
    Right v
     -> return v
    Left e
     | p
     -> pushinner
     | otherwise
     -> Left e
   

tryInserts :: Context -> [Context] -> Maybe [Context]
tryInserts embed intos
 = go intos []
 where
  go [] _
   = Nothing
  go (i:is) rs
    = case insertContext embed i of
       Right c' -> Just  (rs ++ [c'] ++ is)
       Left _   -> go is (rs ++ [i])


-- cannot split appends.
-- but only called by insertContext, which does not take appends anyway.
splitContextIntoApps :: Context -> Either Error (Context,Context)
splitContextIntoApps ctx
 = case ctx of
    ContextRate{}
     | Just (tl, tr) <- takeAppend $ contextRate ctx
     -> do      (lis, ris) <- unzip <$> mapM splitContextIntoApps (contextInner ctx)
                return ( ctx { contextRate      = tl
                             , contextInner     = lis }
                       , ctx { contextRate      = tr
                             , contextInner     = ris } )

    ContextSelect{}
     | Just (tl, tr) <- takeAppend $ contextOuterRate ctx
     -> do      (lis, ris) <- unzip <$> mapM splitContextIntoApps (contextInner ctx)
                return ( ctx { contextOuterRate  = tl
                             , contextInner      = lis }
                       , ctx { contextOuterRate  = tr
                             , contextInner      = ris } )

    ContextSegment{}
     | Just (tl, tr) <- takeAppend $ contextOuterRate ctx
     -> do      (lis, ris) <- unzip <$> mapM splitContextIntoApps (contextInner ctx)
                return ( ctx { contextOuterRate  = tl
                             , contextInner      = lis }
                       , ctx { contextOuterRate  = tr
                             , contextInner      = ris } )


    _
     | otherwise
     -> Left ErrorCannotSplitContext


 where
  takeAppend ty
   | Just (NameTyConFlow TyConFlowRateAppend, [tK, tL])
            <- takePrimTyConApps ty
   = Just (tK, tL)
   | otherwise
   = Nothing

