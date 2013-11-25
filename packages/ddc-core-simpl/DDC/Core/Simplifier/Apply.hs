
-- | Application of simplifiers to modules and expressions.
module DDC.Core.Simplifier.Apply
        ( applySimplifier
        , applyTransform
        , applySimplifierX
        , applyTransformX)
where
import DDC.Base.Pretty
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Fragment
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.Snip          as Snip
import DDC.Core.Transform.Flatten
import DDC.Core.Transform.Beta
import DDC.Core.Transform.Eta           as Eta
import DDC.Core.Transform.Prune
import DDC.Core.Transform.Forward       as Forward
import DDC.Core.Transform.Bubble
import DDC.Core.Transform.Inline
import DDC.Core.Transform.Namify
import DDC.Core.Transform.Rewrite
import DDC.Core.Transform.Elaborate
import DDC.Type.Env                     (KindEnv, TypeEnv)
import Data.Typeable                    (Typeable)
import Control.Monad.State.Strict
import qualified DDC.Base.Pretty        as P
import qualified Data.Set               as Set


-- Modules --------------------------------------------------------------------
-- | Apply a simplifier to a module.
--
--   The state monad can be used by `Namifier` functions to generate fresh names.
--
applySimplifier 
        :: (Show a, Ord n, Show n, Pretty n) 
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Simplifier s a n     -- ^ Simplifier to apply
        -> Module a n           -- ^ Module to simplify
        -> State s (TransformResult (Module a n))

applySimplifier !profile !kenv !tenv !spec !mm
 = let down = applySimplifier profile kenv tenv
   in  case spec of
        Seq t1 t2
         -> do  tm  <- down t1 mm
                tm' <- down t2 (result tm)

                let info =
                        case (resultInfo tm, resultInfo tm') of
                        (TransformInfo i1, TransformInfo i2) -> SeqInfo i1 i2
                
                let again    = resultAgain    tm || resultAgain    tm'
                let progress = resultProgress tm || resultProgress tm'

                return TransformResult
                        { result         = result tm'
                        , resultAgain    = again
                        , resultProgress = progress
                        , resultInfo     = TransformInfo info }

        Fix i s
         -> do  tm      <- applyFixpoint profile kenv tenv i s mm
                let info =
                        case resultInfo tm of
                        TransformInfo info1 -> FixInfo i info1
                
                return TransformResult
                        { result         = result tm
                        , resultAgain    = resultAgain    tm
                        , resultProgress = resultProgress tm
                        , resultInfo     = TransformInfo info }

        Trans t1
         -> applyTransform profile kenv tenv t1 mm


-- | Apply a transform until it stops progressing, or a maximum number of times
applyFixpoint
        :: (Show a, Ord n, Show n, Pretty n)
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Int                  -- ^ Maximum number of times to apply
        -> Simplifier s a n      -- ^ Transform to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (TransformResult (Module a n))

applyFixpoint !profile !kenv !tenv !i' !spec !mm'
 = go i' mm' False
 where
  simp = applySimplifier profile kenv tenv spec

  go 0 mm progress 
   = do tm  <- simp mm
        return tm { resultProgress = progress }

  go i mm progress 
   = do tm  <- simp mm
        case resultAgain tm of
         False 
          ->    return tm { resultProgress = progress }

         True  
          -> do tm' <- go (i-1) (result tm) True

                let info 
                     = case (resultInfo tm, resultInfo tm') of
                        (TransformInfo i1, TransformInfo i2)
                          -> SeqInfo i1 i2

                return TransformResult
                        { result         = result tm'
                        , resultAgain    = resultProgress tm'
                        , resultProgress = resultProgress tm'
                        , resultInfo     = TransformInfo info }

-- | Apply a transform to a module.
applyTransform
        :: (Show a, Ord n, Show n, Pretty n)
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Transform s a n      -- ^ Transform to apply.
        -> Module a n           -- ^ Module to simplify.
        -> State s (TransformResult (Module a n))

applyTransform !profile !_kenv !_tenv !spec !mm
 = let  res x = return $ resultDone (show $ ppr spec) x
   in case spec of
        Id               -> res mm
        Anonymize        -> res $ anonymizeX mm
        Snip config      -> res $ snip config mm
        Flatten          -> res $ flatten mm

        Beta config      
         -> return $ betaReduce    profile config mm

        Eta  config      
         -> return $ Eta.etaModule profile config mm

        Forward          
         -> let config  = Forward.Config (const FloatAllow) False
            in  return $ forwardModule profile config mm

        Bubble           -> res $ bubbleModule mm
        Namify namK namT -> namifyUnique namK namT mm >>= res
        Inline getDef    -> res $ inline getDef Set.empty mm
        Rewrite rules    -> res $ rewriteModule rules mm
        Prune            -> res $ pruneModule profile mm
        Elaborate        -> res $ elaborateModule mm


-- Expressions ----------------------------------------------------------------
-- | Apply a simplifier to an expression.
--
--   The state monad can be used by `Namifier` functions to generate fresh names.
--
applySimplifierX 
        :: (Show a, Show n, Ord n, Pretty n)
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Simplifier s a n     -- ^ Simplifier to apply
        -> Exp a n              -- ^ Expression to simplify
        -> State s (TransformResult (Exp a n))

applySimplifierX !profile !kenv !tenv !spec !xx
 = let down = applySimplifierX profile kenv tenv
   in  case spec of
        Seq t1 t2
         -> do  tx  <- down t1 xx
                tx' <- down t2 (result tx)

                let info =
                        case (resultInfo tx, resultInfo tx') of
                        (TransformInfo i1, TransformInfo i2) -> SeqInfo i1 i2
                
                let again    = resultAgain    tx || resultAgain    tx'
                let progress = resultProgress tx || resultProgress tx'

                return TransformResult
                        { result         = result tx'
                        , resultAgain    = again
                        , resultProgress = progress
                        , resultInfo     = TransformInfo info }

        Fix i s
         -> do  tx      <- applyFixpointX profile kenv tenv i s xx
                let info =
                        case resultInfo tx of
                        TransformInfo info1 -> FixInfo i info1
                
                return TransformResult
                        { result         = result tx
                        , resultAgain    = resultAgain    tx
                        , resultProgress = resultProgress tx
                        , resultInfo     = TransformInfo info }
                
        Trans t1
         -> applyTransformX profile kenv tenv t1 xx


-- | Apply a simplifier until it stops progressing, or a maximum number of times
applyFixpointX
        :: (Show a, Show n, Ord n, Pretty n)
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Int                  -- ^ Maximum number of times to apply
        -> Simplifier s a n     -- ^ Simplifier to apply.
        -> Exp a n              -- ^ Exp to simplify.
        -> State s (TransformResult (Exp a n))

applyFixpointX !profile !kenv !tenv !i' !s !xx'
 = go i' xx' False
 where
  simp = applySimplifierX profile kenv tenv s

  go 0 xx progress 
   = do tx <- simp xx
        return tx { resultProgress = progress }

  go i xx progress 
   = do tx      <- simp xx
        case resultAgain tx of
         False 
          ->    return tx { resultProgress = progress }

         True  
          -> do tx'        <- go (i-1) (result tx) True

                let info 
                     = case (resultInfo tx, resultInfo tx') of
                        (TransformInfo i1, TransformInfo i2) 
                          -> SeqInfo i1 i2

                return TransformResult
                        { result         = result tx'
                        , resultAgain    = resultProgress tx'
                        , resultProgress = resultProgress tx'
                        , resultInfo     = TransformInfo info }


-- | Result of applying two simplifiers in sequence.
data SeqInfo
        = forall i1 i2
        . (Typeable i1, Typeable i2, Pretty i1, Pretty i2)
        => SeqInfo i1 i2
        deriving Typeable


instance Pretty SeqInfo where
 ppr (SeqInfo i1 i2) = ppr i1 P.<> text ";" <$> ppr i2


-- | Result of applying a simplifier until we reach a fixpoint.
data FixInfo
        = forall i1
        . (Typeable i1, Pretty i1)
        => FixInfo Int i1
        deriving Typeable


instance Pretty FixInfo where
 ppr (FixInfo num i1) 
  =  text "fix" <+> int num P.<> text ":"
  <$> indent 4 (ppr i1)


-- | Apply a single transform to an expression.
applyTransformX 
        :: (Show a, Show n, Ord n, Pretty n)
        => Profile n            -- ^ Profile of language we're working in
        -> KindEnv n            -- ^ Kind environment
        -> TypeEnv n            -- ^ Type environment
        -> Transform s a n      -- ^ Transform to apply.
        -> Exp a n              -- ^ Exp  to transform.
        -> State s (TransformResult (Exp a n))

applyTransformX !profile !kenv !tenv !spec !xx
 = let  res x = return $ resultDone (show $ ppr spec) x
   in case spec of
        Id                -> res xx
        Anonymize         -> res    $ anonymizeX xx
        Snip config       -> res    $ snip config xx
        Flatten           -> res    $ flatten xx
        Inline  getDef    -> res    $ inline getDef Set.empty xx

        Beta config       
         -> return $ betaReduce profile config xx

        Eta  config       
         -> return $ Eta.etaX   profile config kenv tenv xx

        Prune             
         -> return $ pruneX     profile kenv tenv xx

        Forward          
         -> let config  = Forward.Config (const FloatAllow) False
            in  return $ forwardX profile config xx

        Bubble            -> res    $ bubbleX kenv tenv xx
        Namify  namK namT -> namifyUnique namK namT xx >>= res
        Rewrite rules     -> return $ rewriteX rules xx
        Elaborate{}       -> res    $ elaborateX xx
    
