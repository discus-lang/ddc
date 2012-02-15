module DDC.Type.Subsumes
        (subsumesT)
where
import DDC.Type.Exp
import DDC.Type.Transform.Crush
import qualified DDC.Type.Sum   as Sum


-- | Check whether the first type subsumes the second.
--
--   This only works for types of Effect and Closure kind.
--   If applied to other type it will return `False`.
subsumesT :: Ord n => Kind n -> Type n -> Type n -> Bool
subsumesT k t1 t2
        | ts1   <- Sum.singleton k $ crushT t1
        , ts2   <- Sum.singleton k $ crushT t2
        = and $ [ Sum.elem t ts1 | t <- Sum.toList ts2 ]

