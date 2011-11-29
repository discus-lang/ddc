
-- | Predicates on type expressions.
module DDC.Type.Predicates
        ( isBot
        , isBottom)
where
import DDC.Type.Exp
import qualified DDC.Type.Sum   as T

-- | Test if some type is a `TBot`. 
isBot :: Type n -> Bool
isBot (TBot _)  = True
isBot _         = False


-- | Test if some type is a `TBot` or an empty `TSum`.
isBottom :: Type n -> Bool
isBottom tt
        | TBot{}        <- tt = True

        | TSum ss       <- tt
        , []            <- T.toList ss
        = True
        
        | otherwise     = False
