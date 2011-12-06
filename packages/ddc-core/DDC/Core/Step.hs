
module DDC.Core.Step where
import DDC.Core.Compounds
import DDC.Core.Exp

-- | Holds a function to perform primitive reductions.
--   These are defined by the client.
data PrimStep a n p s
        = PrimStep
        { primStep      :: p -> [Exp a p n] -> s -> Maybe (s, Exp a p n) }
                

-- | Perform a single step reduction of a core expression.
step :: PrimStep a n p s -> Exp a p n -> s -> Maybe (s, Exp a p n)
step (PrimStep primStep) xx s 
        -- A primitive reduction defined by the client.
        | Just (p, xs)     <- takeXPrimApps xx
        = primStep p xs s
        
        | otherwise
        = Nothing