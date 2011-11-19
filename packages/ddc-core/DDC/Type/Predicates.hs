
module DDC.Type.Predicates
        (isBot)
where
import DDC.Type.Exp

isBot :: Type v c -> Bool
isBot (TBot _)  = True
isBot _         = False
