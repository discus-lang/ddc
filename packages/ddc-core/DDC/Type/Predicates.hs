
-- | Predicates on type expressions.
module DDC.Type.Predicates
        (isBot)
where
import DDC.Type.Exp

isBot :: Type n -> Bool
isBot (TBot _)  = True
isBot _         = False
