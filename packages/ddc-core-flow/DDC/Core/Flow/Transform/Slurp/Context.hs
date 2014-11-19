
module DDC.Core.Flow.Transform.Slurp.Context
    ( insertContext
    , mergeContexts
    , resizeContext
    , contextContainsSelect )
where
import DDC.Core.Flow.Context
import DDC.Core.Flow.Transform.Slurp.Error
import DDC.Core.Flow.Transform.Slurp.Resize
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Process.Operator
import DDC.Core.Compounds.Simple
import DDC.Core.Exp.Simple
import Control.Applicative
import Data.Function (on)
import Data.List     (nubBy)

-- "embed" is to be pushed inside "into"
-- only one of "embed" or "into" can contain inner contexts;
-- otherwise, no promises are made about merging these
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
          -> app into embed


    ContextSelect{}
     -> case embed of
         ContextRate{}
          | contextInnerRate into == contextRate embed
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
          -> app into embed


    ContextSegment{}
     -> case embed of
         ContextRate{}
          | contextInnerRate into == contextRate embed
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
          -> app into embed


    ContextAppend{}
     -> app embed into

 where
  descend =
   case tryInserts embed (contextInner into) of
    Nothing -> Left (ErrorCannotMergeContext embed into)
    Just cs -> return into { contextInner = cs }

  dropops =
   let ops' = contextOps   into ++ contextOps   embed
   in  return
         into { contextOps   = nubBy ((==) `on` bindOfOp) ops'
              , contextInner = contextInner into ++ contextInner embed }

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

  app splittee injectee
   = do (ls, rs) <- splitContextIntoApps splittee
        ls'      <- insertContext ls (contextInner1 injectee)
        rs'      <- insertContext rs (contextInner2 injectee)
        return $ injectee
                 { contextInner1 = ls'
                 , contextInner2 = rs' }

   

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

    ContextAppend{}
     ->         return ( contextInner1 ctx
                       , contextInner2 ctx )

    _
     -> -- Left (ErrorCannotSplitContext ctx) 
        return (ctx, ctx)


 where


mergeContexts :: Context -> Context -> Either Error Context
mergeContexts a b
 = insertContext a b

resizeContext :: Resize -> Context -> Either Error Context
resizeContext resize ctx
 = case resize of
    Id _    
     -> return ctx
    AppL a b
     -> return
         ContextAppend
         { contextRate1  = a
         , contextInner1 = wrapCtx b ctx
         , contextRate2  = b
         , contextInner2 = emptyCtx a
         }
    AppR a b
     -> return
         ContextAppend
         { contextRate1  = a
         , contextInner1 = emptyCtx a
         , contextRate2  = b
         , contextInner2 = wrapCtx b ctx
         }
    App _ k' _ l' ls rs
     | ContextAppend{} <- ctx
     -> do  in1 <- resizeContext ls (contextInner1 ctx)
            in2 <- resizeContext rs (contextInner2 ctx)
            return
             ContextAppend
             { contextRate1  = k'
             , contextInner1 = in1
             , contextRate2  = l'
             , contextInner2 = in2 }
     | otherwise
     -> Left (ErrorCannotResizeContext ctx)
    Sel1 _ k _ r
     -> do  ctx' <- resizeContext r ctx
            return $ wrapCtx k ctx'
    Segd _ k _ r
     -> do  ctx' <- resizeContext r ctx
            return $ wrapCtx k ctx'
    -- TODO doublecheck this after implementing slurp for OpSeriesCross
    Cross _ k _ r
     -> do  ctx' <- resizeContext r ctx
            return $ wrapCtx k ctx'
            
            


emptyCtx :: Type Name -> Context
emptyCtx k
 = ContextRate
 { contextRate  = k
 , contextInner = []
 , contextOps   = [] }

wrapCtx :: Type Name -> Context -> Context
wrapCtx k ctx
 = case ctx of
   ContextRate{}
    | contextRate ctx == k
    -> ctx

   ContextAppend{}
    | Just (l,r) <- takeAppend k
    , contextRate1 ctx == l
    , contextRate2 ctx == r
    -> ctx
    
   _
    | otherwise
    -> ContextRate
       { contextRate  = k
       , contextInner = [ctx]
       , contextOps   = [] }

takeAppend ty
 | Just (NameTyConFlow TyConFlowRateAppend, [tK, tL])
          <- takePrimTyConApps ty
 = Just (tK, tL)
 | otherwise
 = Nothing


contextContainsSelect :: Context -> Type Name -> Bool
contextContainsSelect ctx k
 = case ctx of
    ContextRate{}
     -> inners
    ContextSelect{}
     -> contextInnerRate ctx == k
     || inners
    ContextSegment{}
     -> inners
    ContextAppend{}
     -> contextContainsSelect (contextInner1 ctx) k
     || contextContainsSelect (contextInner2 ctx) k
 where
  inners = any (flip contextContainsSelect k) (contextInner ctx)

