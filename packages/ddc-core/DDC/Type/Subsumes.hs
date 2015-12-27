module DDC.Type.Subsumes
        (subsumesT)
where
import DDC.Type.Exp
import DDC.Type.Predicates
import DDC.Type.Transform.Crush
import qualified DDC.Type.Sum   as Sum


-- | Check whether the first type subsumes the second.
--
--   Both arguments are converted to sums, and we check that every
--   element of the second sum is equivalent to an element in the first.
--
--   This only works for well formed types of effect and closure kind.
--   Other types will yield `False`.
subsumesT :: Ord n => Kind n -> Type n -> Type n -> Bool
subsumesT k t1 t2
        | isEffectKind k
        , ts1       <- Sum.singleton k $ crushEffect t1
        , ts2       <- Sum.singleton k $ crushEffect t2
        = and $ [ Sum.elem t ts1 | t <- Sum.toList ts2 ]

        | otherwise
        = False
