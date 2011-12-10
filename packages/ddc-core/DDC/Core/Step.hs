
module DDC.Core.Step where
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Pretty
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
        -> s                    -- ^ Current store.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (s, Exp a n)   -- ^ New store and expression.

step p store xx
-- = trace (show $ text "stepping: " <> text (show xx) <> line)
 = step' p store xx
 
step' (PrimStep primStep) store xx
        -- A primitive reduction defined by the client.
        | Just (p, xs)          <- takeXPrimApps xx
        , Just (store', x')     <- primStep p xs store
        = Just (store', x')

step' p store (XApp a x1 x2)
        | Just (store', x1')    <- step p store x1
        = Just (store', XApp a x1' x2)

step' p store (XApp a x1 x2)
        | Just (store', x2')    <- step p store x2
        = Just (store', XApp a x1 x2')
        
step' _ _ _        
        = Nothing
        
        
-- | Check if an expression is a weak normal form.
isWnf :: Exp a n -> Bool
isWnf xx
 = case xx of
         XVar{}         -> True
         XCon{}         -> True
         XApp{}         -> False
         XLam{}         -> True
         XLet{}         -> False
         XCase{}        -> False
         XCast _ x _    -> isWnf x
         XType{}        -> True
         XWitness{}     -> True