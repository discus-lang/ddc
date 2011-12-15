
module DDC.Core.Step 
        ( PrimStep       (..)
        , step
        , isWnf
        , regionWitnessOfType )
where
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
        , primNewRegion :: s -> (s, Bound n) 

        -- | Get the arity of a primop, where the arity includes the type and witness arguments.
        , primArity     :: n -> Maybe Int }
                

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
        | isWnf (primArity ps) x1
        , Just (store', x1')    <- step ps store x1
        = Just (store', XApp a x1' x2)

-- (EvApp2): Evaluate the right of an application.
step' ps store (XApp a x1 x2)
        | isWnf (primArity ps) x1
        , Just (store', x2')    <- step ps store x2
        = Just (store', XApp a x1 x2')

-- (EvLetSubst)
step' ps store (XLet _ (LLet b x1) x2)
        | isWnf (primArity ps) x1
        = case takeSubstBoundOfBind b of
           Nothing      -> Just (store, x2)
           Just u       -> Just (store, substituteX u x1 x2)

-- (EvLetStep)
step' ps store (XLet a (LLet b x1) x2)
        | Just (store', x1')    <- step ps store x1
        = Just (store', XLet a (LLet b x1') x2)

-- (EvCreateRegion): Create a new region.
step' ps store (XLet a (LLetRegion bRegion bws) x)
 | Just uRegion <- takeSubstBoundOfBind bRegion
 = let  
        -- Allocation a new region handle for the bound region.
        (store', uHandle) = primNewRegion ps store
        tHandle = TCon $ TyConBound uHandle

        -- Substitute handle into the witness types.
        bws'    = map (substituteT uRegion tHandle) bws

        -- Build witnesses for each of the witness types.
        uws'    = [(u, t) | Just u <- map takeSubstBoundOfBind bws'
                          | Just t <- map regionWitnessOfType  $ map typeOfBind bws']
        
        -- Substitute handle and witnesses into body.
        x'      = substituteT  uRegion tHandle
                $ substituteWs uws' x

   in   Just (store', XLet a (LWithRegion uHandle) x')

 | otherwise
 = Just (store, x)

-- (EvEjectRegion): Eject completed value from the region context.
step' ps store (XLet _ (LWithRegion _) x)
        | isWnf (primArity ps) x
        = Just (store, x)
 
-- (EvWithRegion): Reduction within a region context.
step' ps store (XLet a (LWithRegion uRegion) x)
        | Just (store', x')     <- step ps store x
        = Just (store', XLet a (LWithRegion uRegion) x')


-- (Done/Stuck): Either a value, or a stuck expression.
step' _ _ _        
        = Nothing
        
        
-- isWnf ----------------------------------------------------------------------
-- | Check if an expression is a weak normal form.
isWnf :: (n -> Maybe Int) -> Exp a n -> Bool
isWnf pArity xx
 = case xx of
         XVar{}         -> True
         XCon{}         -> True
         XLam{}         -> True
         XLet{}         -> False
         XCase{}        -> False
         XCast _ x _    -> isWnf pArity x
         XType{}        -> True
         XWitness{}     -> True

         XApp{} 
          | Just (p, xs)        <- takeXPrimApps xx
          , Just arity          <- pArity p
          , length xs < arity
          -> True
          
          | otherwise
          -> False


-- | Get the region witness corresponding to one of the witness types that are
--   permitted in a letregion.
regionWitnessOfType :: Type n -> Maybe (Witness n)
regionWitnessOfType tt
 = case tt of
        TApp (TCon (TyConWitness TwConMutable)) r 
         -> Just $ WApp (WCon (WiConMutable)) (WType r)
        
        TApp (TCon (TyConWitness TwConConst)) r
         -> Just $ WApp (WCon (WiConConst))   (WType r)

        _ -> Nothing

