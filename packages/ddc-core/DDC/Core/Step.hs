
module DDC.Core.Step where
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Type.Env             (Env)
import DDC.Base.Pretty          ()
--import Debug.Trace

-- | Holds a function to perform primitive reductions.
--   These are defined by the client.
data PrimStep a n s
        = PrimStep
        { primStep      :: n -> [Exp a n] -> s -> Maybe (s, Exp a n) }
                

-- | Perform a single step reduction of a core expression.
step    :: (Eq n, Pretty n, Show a, Show n) 
        => PrimStep a n s       -- ^ Function to step primitive ops.
        -> Env n                -- ^ Type environment, to check which ops are primitive.
        -> Exp a n              -- ^ Expression to check.
        -> s                    -- ^ Current store.
        -> Maybe (s, Exp a n)   -- ^ New store and expression.
step p env x store
-- = trace (show $ text "stepping: " <> text (show x))
 = step' p env x store
         
step' (PrimStep primStep) _env xx store
        -- A primitive reduction defined by the client.
        | Just (p, xs)     <- takeXPrimApps xx
        = primStep p xs store
        
        | otherwise
        = Nothing