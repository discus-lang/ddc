
-- | Single step evalation for the DDC core language.
--
--   These are the rules for the plain calculus. The rules for primitive
--   operators and constructors are defined in the interpreter package, 
--   as they depend on the exact representation of the store.
--
module DDCI.Core.Eval.Step 
        ( step
        , isWnf
        , regionWitnessOfType )
where
import DDCI.Core.Eval.Store
import DDCI.Core.Eval.Name
import DDCI.Core.Eval.Prim
import DDCI.Core.Eval.Env
import DDC.Core.Compounds
import DDC.Core.Transform
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Base.Pretty          ()
                

-- step -----------------------------------------------------------------------
-- | Perform a single step reduction of a core expression.
step    :: Store                      -- ^ Current store.
        -> Exp () Name                -- ^ Expression to check.
        -> Maybe (Store, Exp () Name) -- ^ New store and expression.

step store xx
 --  trace (show $ text "stepping: " <> text (show xx) <> line)
 = step' store xx
 

-- (EvPrim): Step a primitive operator or constructor defined by the client.
step' store xx
        | Just (p, xs)          <- takeXPrimApps xx
        , Just (store', x')     <- primStep p xs store
        = Just (store', x')


-- (EvApp1): Evaluate the left of an application.
step' store (XApp a x1 x2)
        | isWnf x1
        , Just (store', x1')    <- step store x1
        = Just (store', XApp a x1' x2)


-- (EvApp2): Evaluate the right of an application.
step' store (XApp a x1 x2)
        | isWnf x1
        , Just (store', x2')    <- step store x2
        = Just (store', XApp a x1 x2')


-- (EvLetSubst): Substitute in a bound value in a let expression.
step' store (XLet _ (LLet b x1) x2)
        | isWnf x1
        = case takeSubstBoundOfBind b of
           Nothing      -> Just (store, x2)
           Just u       -> Just (store, substituteX u x1 x2)


-- (EvLetStep): Step the binding in a let-expression.
step' store (XLet a (LLet b x1) x2)
        | Just (store', x1')    <- step store x1
        = Just (store', XLet a (LLet b x1') x2)


-- (EvCreateRegion): Create a new region.
step' store (XLet a (LLetRegion bRegion bws) x)
 | Just uRegion <- takeSubstBoundOfBind bRegion
 = let  
        -- Allocation a new region handle for the bound region.
        (store', uHandle) = primNewRegion store
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


-- (EvEjectRegion): Eject completed value from the region context, and delete the region.
step' store (XLet _ (LWithRegion r) x)
        | isWnf x
        , Just store'    <- primDelRegion r store
        = Just (store', x)

 
-- (EvWithRegion): Reduction within a region context.
step' store (XLet a (LWithRegion uRegion) x)
        | Just (store', x')     <- step store x
        = Just (store', XLet a (LWithRegion uRegion) x')


-- (Done/Stuck): Either already a value, or expression is stuck.
step' _ _        
        = Nothing
        
        
-- isWnf ----------------------------------------------------------------------
-- | Check if an expression is a weak normal form (a value).
--   This is not /strong/ normal form because we don't require expressions
--   under lambdas to also be values.
isWnf :: Exp a Name -> Bool
isWnf xx
 = case xx of
         XVar{}         -> True
         XCon{}         -> True
         XLam{}         -> True
         XLet{}         -> False
         XCase{}        -> False
         XCast _ _ x    -> isWnf x
         XType{}        -> True
         XWitness{}     -> True

         XApp{} 
          | Just (p, xs)        <- takeXPrimApps xx
          , Just arity          <- arityOfPrimName p
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

