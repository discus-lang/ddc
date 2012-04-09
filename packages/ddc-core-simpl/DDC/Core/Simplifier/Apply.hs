
-- | Application of simplifiers to modules and expressions.
module DDC.Core.Simplifier.Apply
        ( applySimplifier
        , applyTransform

        , applySimplifierX
        , applyTransformX)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Simplifier.Base
import DDC.Core.Transform.AnonymizeX
import DDC.Core.Transform.Snip
import DDC.Core.Transform.Flatten
import DDC.Core.Transform.Beta
import DDC.Core.Transform.Rewrite
import DDC.Core.Transform.Rewrite.Rule
import DDC.Core.Transform.Namify
import Control.Monad.State.Strict


-- Modules --------------------------------------------------------------------
-- | Apply a simplifier to a module.
applySimplifier 
        :: (Show a, Ord n) 
        => Simplifier -> Module a n -> Module a n

applySimplifier spec mm
 = case spec of
        Seq t1 t2      
         -> applySimplifier t2 (applySimplifier t1 mm)

        Trans t1
         -> applyTransform t1 mm


-- | Apply a transform to a module.
applyTransform
        :: (Show a, Ord n)
        => Transform -> Module a n -> Module a n

applyTransform spec mm
 = case spec of
        Id              -> mm
        Anonymize       -> anonymizeX mm
        Snip            -> snip mm
        Flatten         -> flatten mm
        _               -> error "applyTransform: finish me"


-- Expressions ----------------------------------------------------------------
-- | Apply a simplifier to an expression.
applySimplifierX 
        :: (Show a, Show n, Ord n)
        => Simplifier 
        -> [RewriteRule a n] 
        -> Namifier s n
        -> Namifier s n
        -> Exp a n    
        -> State s (Exp a n)

applySimplifierX spec rules namK namT xx
 = case spec of
        Seq t1 t2
         -> do  xx'     <- applySimplifierX t1 rules namK namT xx
                applySimplifierX t2 rules namK namT xx'

        Trans t1
         -> applyTransformX  t1 rules namK namT xx


-- | Apply a transform to an expression.
applyTransformX 
        :: (Show a, Show n, Ord n)
        => Transform  
        -> [RewriteRule a n] 
        -> Namifier s n
        -> Namifier s n
        -> Exp a n    
        -> State s (Exp a n)

applyTransformX spec rules namK namT xx
 = case spec of
        Id              -> return xx
        Anonymize       -> return $ anonymizeX xx
        Snip            -> return $ snip xx
        Flatten         -> return $ flatten xx
        Beta            -> return $ betaReduce xx
        Rewrite         -> return $ rewrite rules xx
        Namify          -> namify namK namT xx


