
module DDC.Core.Step where
import DDC.Core.Compounds
import DDC.Core.Exp

-- | Holds a function to perform primitive reductions.
--   These are defined by the client.
data PrimStep a n s
        = PrimStep
        { primStep      :: n -> [Exp a n] -> s -> Maybe (s, Exp a n) }
                

-- | Perform a single step reduction of a core expression.
step :: PrimStep a n s -> Exp a n -> s -> Maybe (s, Exp a n)
step (PrimStep primStep) xx s 
        -- A primitive reduction defined by the client.
        | Just (p, xs)     <- takeXPrimApps xx
        = primStep p xs s
        
        | otherwise
        = Nothing