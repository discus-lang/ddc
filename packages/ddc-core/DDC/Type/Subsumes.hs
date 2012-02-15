module DDC.Type.Subsumes
        (subsumesT)
where
import DDC.Type.Exp
import DDC.Type.Transform.Crush
import qualified DDC.Type.Sum   as Sum


-- | Check whether the first type subsumes the second.
--
--   Both arguments are converted to sums, and we check that every
--   element of the second sum is equivalent to an element in the first.
subsumesT :: Ord n => Kind n -> Type n -> Type n -> Bool
subsumesT k t1 t2
        | ts1   <- Sum.singleton k $ crushT t1
        , ts2   <- Sum.singleton k $ crushT t2
        = and $ [ Sum.elem t ts1 | t <- Sum.toList ts2 ]

