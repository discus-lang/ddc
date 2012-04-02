
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
        => Simplifier -> [RewriteRule a n] 
        -> Exp a n    -> Exp a n

applySimplifierX spec rules xx
 = case spec of
        Seq t1 t2
         -> applySimplifierX t2 rules (applySimplifierX t1 rules xx)

        Trans t1
         -> applyTransformX t1 rules xx


-- | Apply a transform to an expression.
applyTransformX 
        :: (Show a, Show n, Ord n)
        => Transform  -> [RewriteRule a n] 
        -> Exp a n    -> Exp a n

applyTransformX spec rules xx
 = case spec of
        Id              -> xx
        Anonymize       -> anonymizeX xx
        Snip            -> snip xx
        Flatten         -> flatten xx
        Beta            -> betaReduce xx
        Rewrite         -> rewrite rules xx
