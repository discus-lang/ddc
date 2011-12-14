
module DDC.Core.Step where
import DDC.Core.Compounds
import DDC.Core.Transform
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Core.Pretty
import DDC.Base.Pretty          ()
-- import Debug.Trace

-- | Holds a function to perform primitive reductions.
--   These are defined by the client.
data PrimStep a n s
        = PrimStep
        { primStep      :: n -> [Exp a n] -> s -> Maybe (s, Exp a n)
        , primNewRegion :: s -> (s, Bound n) }
                

-- step -----------------------------------------------------------------------
-- | Perform a single step reduction of a core expression.
step    :: (Eq n, Ord n, Pretty n, Show a, Show n) 
        => PrimStep a n s       -- ^ Function to step primitive ops.
        -> s                    -- ^ Current store.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (s, Exp a n)   -- ^ New store and expression.

step p store xx
 --  trace (show $ text "stepping: " <> text (show xx) <> line)
 = step' p store xx
 
-- (EvPrim)
step' ps store xx
        -- A primitive reduction defined by the client.
        | Just (p, xs)          <- takeXPrimApps xx
        , Just (store', x')     <- primStep ps p xs store
        = Just (store', x')

-- (EvApp1): Evaluate the left of an application.
step' ps store (XApp a x1 x2)
        | isWnf x1
        , Just (store', x1')    <- step ps store x1
        = Just (store', XApp a x1' x2)

-- (EvApp2): Evaluate the right of an application.
step' ps store (XApp a x1 x2)
        | isWnf x1
        , Just (store', x2')    <- step ps store x2
        = Just (store', XApp a x1 x2')

-- (EvLetDone)
step' ps store (XLet a (LLet b x1) x2)
        | Just (store', x1')    <- step ps store x1
        = Just (store', XLet a (LLet b x1') x2)

-- (EvCreateRegion): Create a new region.
-- TODO: substitute in witnesses
step' ps store (XLet a (LLetRegion bRegion _bws) x)
 | Just uRegion <- takeSubstBoundOfBind bRegion
 = let  (store', uHandle) = primNewRegion ps store
        x'      = substituteT uRegion (TCon $ TyConBound uHandle) x
   in   Just (store', XLet a (LWithRegion uHandle) x')

 | otherwise
 = Just (store, x)
 
-- (EvWithRegion): Reduction within a region context.
step' ps store (XLet a (LWithRegion uRegion) x)
        | Just (store', x')     <- step ps store x
        = Just (store', XLet a (LWithRegion uRegion) x')

-- (EvEjectRegion): Eject completed value from the region context.
step' _ store (XLet _ (LWithRegion _) x)
        | isWnf x
        = Just (store, x)

-- (Done/Stuck): Either a value, or a stuck expression.
step' _ _ _        
        = Nothing
        
        
-- isWnf ----------------------------------------------------------------------
-- | Check if an expression is a weak normal form.
isWnf :: Exp a n -> Bool
isWnf xx
 = case xx of
         XVar{}         -> True
         XCon{}         -> True
         XLam{}         -> True
         XLet{}         -> False
         XCase{}        -> False
         XCast _ x _    -> isWnf x
         XType{}        -> True
         XWitness{}     -> True
         
         XApp{} 
          | Just (_, xs)        <- takeXPrimApps xx
          , and $ map isWnf xs
          -> True
          
          | otherwise
          -> False
