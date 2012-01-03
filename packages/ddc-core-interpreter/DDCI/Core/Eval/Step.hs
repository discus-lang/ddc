
-- | Single step evalation for the DDC core language.
--
--   These are the rules for the plain calculus. The rules for primitive
--   operators and constructors are defined in the interpreter package, 
--   as they depend on the exact representation of the store.
--
module DDCI.Core.Eval.Step 
        ( step
        , StepResult(..)
        , isWnf
        , regionWitnessOfType )
where
import DDCI.Core.Eval.Store
import DDCI.Core.Eval.Name
import DDCI.Core.Eval.Prim
import DDCI.Core.Eval.Env
import DDCI.Core.Eval.Compounds
import DDC.Core.Check
import DDC.Core.Transform
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Type.Compounds
import qualified Data.Set       as Set
import Debug.Trace

-- StepResult -----------------------------------------------------------------
-- | The result of stepping some expression.
data StepResult
        -- | Expression transitioned to a new state.
        = StepProgress Store (Exp () Name)

        -- | Expression cannot step, but it is wnf.
        --   We're done already.
        | StepDone

        -- | Expression cannot step, and is not-wnf.
        --   The original expression was mistyped,
        --   or something is wrong with the interpreter.
        | StepStuck

        -- | Expression is stuck, and we know for sure it's mistyped.
        --   This can happen in the EvLam rule.
        | StepStuckMistyped (Error () Name)

        -- | Expression is stuck, and it's because we found some letrec
        --   bindings that were not lambdas.
        | StepStuckLetrecBindsNotLam
        deriving (Show)


-- step -----------------------------------------------------------------------
-- | Perform a single step reduction of a core expression.
step    :: Store        -- ^ Current store.
        -> Exp () Name  -- ^ Expression to step.
        -> StepResult   -- ^ Result of stepping it.

-- TODO: split casts off the front.
step store xx
  = step' store xx


-- (EvPrim)
-- Step a primitive operator or constructor defined by the client.
step' store xx
        | Just (p, xs)          <- takeXPrimApps xx
        , and $ map (isWnf store) xs
        , Just (store', x')     <- stepPrimOp p xs store
        = StepProgress store' x'


-- (EvLam)
-- Add abstractions to the heap.
step' store xx@XLam{}
 = let  Just (bs, xBody)        = takeXLams xx
        (store', l)             = allocBind (Rgn 0) (SLams bs xBody) store

        -- We need the type of the expression to attach to the location
        -- This fakes the store typing from the formal typing rules.
   in   case typeOfExp primDataDefs xx of
         Left err  -> StepStuckMistyped err
         Right t   -> StepProgress store' (XCon () (UPrim (NameLoc l) t))


-- (EvAlloc)
-- Construct some data in the heap.
step' store xx
        | Just (u, xs)          <- takeXConApps xx
        , case u of
            UName NameCon{}     _               -> True
            UPrim (NamePrimCon PrimDaConUnit) _ -> False
            UPrim NamePrimCon{} _               -> True
            UPrim NameInt{}     _               -> True
            _                                   -> False
        , and $ map (isWnf store) xs
        = case u of
           UPrim n _       
            | Just (store', x') <- stepPrimCon n xs store
            -> StepProgress store' x'

           -- TODO: handle non prim constructors.
           _  -> trace "step' (EvAlloc): not a primcon or something broken"
               $ StepStuck


-- (EvAppArgs)
-- Step the left-most non-wnf argument of a lambda.
step' store xx
        | xL1 : xsArgs  <- takeXApps xx
        , Just l1       <- takeLocX xL1
        , Just (Rgn 0, SLams bs _xBody)  <- lookupRegionBind l1 store

        -- See if an arg to any of the lambdas needs to be stepped.
        , arity         <- length bs
        , wnfs          <- map (isWnf store) xsArgs
        , or (take arity $ map not wnfs) 

        = let -- Step the first non-wnf argument.        
              --  This should never error, as we took as many args
              --  as we had wnf flags.
              stepArg _          []     
               = error "stepArg: no more args"

              stepArg []         xs
               = Right (store, xs)

              stepArg (True:ws)  (x:xs)  
               = case stepArg ws xs of
                  Right (store', xs')    -> Right (store', x : xs')
                  Left err               -> Left err 

              stepArg (False:_)  (x:xs)
               = case step store x of
                  StepProgress store' x' -> Right (store', x' : xs)
                  err                    -> Left err

           in case stepArg wnfs xsArgs of
                Left err        -> err
                Right (store2, xsArgs')
                 -> StepProgress store2 (makeXApps () xL1 xsArgs')


-- (EvAppSubst)
-- Substitute wnf arguments into an abstraction.
step' store xx
        | xL1 : xsArgs  <- takeXApps xx
        , Just l1       <- takeLocX xL1
        , Just (Rgn 0, SLams bs xBody) <- lookupRegionBind l1 store

        -- Take as many wnfs as possible to satisfy binders.
        , arity                 <- length bs
        , (wnfs, nonWnfs)       <- span (isWnf store) xsArgs

        -- If we have any wnfs at all, then we can do a substitution.
        , not $ null wnfs

        = let argsToSubst     = take arity wnfs
              argsOverApplied = drop (length argsToSubst) wnfs
              argsLeftover    = argsOverApplied ++ nonWnfs
                
              bsToSubst       = take (length argsToSubst) bs
              bsLeftover      = drop (length bsToSubst)   bs

              xResult         = substituteXArgs (zip bsToSubst argsToSubst)
                              $ makeXLams () bsLeftover xBody

          in  StepProgress store (makeXApps () xResult argsLeftover)


-- (EvApp2)
-- Evaluate the right of an application.
step' store (XApp a x1 x2)
 | isWnf store x1
 = case step store x2 of
    StepProgress store' x2' -> StepProgress store' (XApp a x1 x2')
    err                     -> err


-- (EvApp1)
-- Evaluate the left of an application.
step' store (XApp a x1 x2)
 = case step store x1 of
    StepProgress store' x1' -> StepProgress store' (XApp a x1' x2)
    err                     -> err


-- (EvLetSubst)
-- Substitute in a bound value in a let expression.
step' store (XLet _ (LLet b x1) x2)
        | isWnf store x1
        = StepProgress store (substituteX b x1 x2)


-- (EvLetStep)
-- Step the binding in a let-expression.
step' store (XLet a (LLet b x1) x2)
 = case step store x1 of
    StepProgress store' x1' -> StepProgress store' (XLet a (LLet b x1') x2)
    err                     -> err


-- (EvLetRec)
-- Add recursive bindings to the store.
step' store (XLet _ (LRec bxs) x2)
 = let  (bs, xs) = unzip bxs
        ts       = map typeOfBind bs

        -- Allocate new locations in the store to hold the expressions.
        (store1, ls)  = newLocs (length bs) store
        xls           = [XCon () (UPrim (NameLoc l) t) | (l, t) <- zip ls ts]

        -- Substitute locations into all the bindings.
        xs'      = map (substituteXs (zip bs xls)) xs


        -- Create store objects for each of the bindings.
        mos      = map (\x -> case takeXLams x of
                               Just (bs', xBody) -> Just $ SLams bs' xBody
                               _                 -> Nothing)
                       xs'

      -- If this fails then some of the bindings did not have lambdas out the
      -- front. We don't support plain value recursion yet.
   in case sequence mos of
       Nothing        -> StepStuckLetrecBindsNotLam
       Just os
        -> let -- Add all the objects to the store.
               store2   = foldr (\(l, o) -> addBind l (Rgn 0) o) store1
                        $ zip ls os
        
               -- Substitute locations into the body expression.
               x2'      = substituteXs (zip bs xls) x2

           in  StepProgress store2 x2'


-- (EvCreateRegion)
-- Create a new region.
step' store (XLet a (LLetRegion bRegion bws) x)
        | Just uRegion  <- takeSubstBoundOfBind bRegion

        -- Allocate a new region handle for the bound region.
        , (store1, uHandle@(UPrim (NameRgn rgn) _))
                        <- primNewRegion store
        , tHandle       <- TCon $ TyConBound uHandle

        -- Substitute handle into the witness types.
        , bws'          <- map (substituteBoundT uRegion tHandle) bws

        -- Build witnesses for each of the witness types.
        -- This can fail if the set of witness signatures is malformed.
        , Just wits     <- sequence 
                        $  map regionWitnessOfType
                        $  map typeOfBind bws'

        = let   -- Substitute handle and witnesses into body.
                x'      = substituteBoundT  uRegion tHandle
                        $ substituteWs (zip bws' wits)  x

                isGlobalBind b
                 = case typeOfBind b of
                    TApp (TCon (TyConWitness TwConGlobal)) _ 
                        -> True
                    _   -> False

                -- Set region to global if there is a witness for it.
                store2  = if or $ map isGlobalBind bws
                                then setGlobal rgn store1
                                else store1

          in    StepProgress store2 (XLet a (LWithRegion uHandle) x')

        -- Region binder was a wildcard, so we can't create the region handle.
        --  No witness sigs can be in the set, because any sig would need
        --  to reference the region variable. Just create a dummy region in the
        --  store to simulate what would happen if there was a real binder.
        | otherwise
        = let   (store', _)     = primNewRegion store
          in    StepProgress store' x


-- (EvEjectRegion)
--  Eject completed value from the region context, and delete the region.
step' store (XLet _ (LWithRegion r@(UPrim (NameRgn rgn) _)) x)
        | isWnf store x
        , Set.member rgn (storeGlobal store)
        = StepProgress store x

        | isWnf store x
        , Just store'    <- primDelRegion r store
        = StepProgress store' x

 
-- (EvWithRegion)
-- Reduction within a region context.
step' store (XLet a (LWithRegion uRegion) x)
 = case step store x of
    StepProgress store' x' 
          -> StepProgress store' (XLet a (LWithRegion uRegion) x')
    err   -> err


-- (EvCaseMatch)
-- Case branching.
step' store (XCase a xDiscrim alts)
        | Just lDiscrim            <- takeLocX xDiscrim
        , Just (SObj nTag lsArgs)  <- lookupBind lDiscrim store
        , AAlt pat xBody : _       <- filter (tagMatchesAlt nTag) alts
        = case pat of
           PDefault         
            -> StepProgress store xBody

           PData _ bsArgs      
            | tsArgs    <- map typeOfBind bsArgs
            , bxsArgs   <- [ (b, XCon a (UPrim (NameLoc l) t))
                                | l     <- lsArgs
                                | t     <- tsArgs
                                | b     <- bsArgs]
            -> StepProgress store
                    (substituteXs bxsArgs xBody)


-- (EvCaseStep)
-- Evaluation of discriminant.
step' store (XCase a xDiscrim alts)
 = case step store xDiscrim of
    StepProgress store' xDiscrim' 
          -> StepProgress store' (XCase a xDiscrim' alts)
    err   -> err


-- (EvPurifyEject)
-- Eject values from purify casts as there are no more effects to be had.
step' store (XCast _ (CastPurify _) x)
        | isWnf store x
        = StepProgress store x


-- (EvCast)
-- Evaluate under casts.
step' store (XCast a cc x)
 = case step store x of
    StepProgress store' x'
          -> StepProgress store' (XCast a cc x')
    err   -> err



-- (Done/Stuck)
-- Either already a value, or expression is stuck.
step' store xx
 | isWnf store xx       = StepDone
 | otherwise            = StepStuck
        


-- Alternatives ---------------------------------------------------------------
-- | See if a constructor tag matches a case alternative.
tagMatchesAlt :: Name -> Alt a Name -> Bool
tagMatchesAlt n (AAlt p _)
        = tagMatchesPat n p


-- | See if a constructor tag matches a pattern.
tagMatchesPat :: Name -> Pat Name -> Bool
tagMatchesPat _ PDefault        = True
tagMatchesPat n (PData u' _)
 = case takeNameOfBound u' of
        Just n' -> n == n'
        _       -> False

        
-- isWnf ----------------------------------------------------------------------
-- | Check if an expression is a weak normal form (a value).
--   This is not /strong/ normal form because we don't require expressions
--   under lambdas to also be values.
isWnf :: Store -> Exp a Name -> Bool
isWnf store xx
 = case xx of
         XVar{}         -> True
         XCon{}         -> True
         XLam{}         -> False
         XLet{}         -> False
         XCase{}        -> False
         XCast _ _ x    -> isWnf store x
         XType{}        -> True
         XWitness{}     -> True

         XApp _ x1 x2

          -- Application if a primop to enough args is not wnf.
          | Just (n, xs)     <- takeXPrimApps xx
          , and $ map (isWnf store) xs
          , Just a           <- arityOfName n
          , length xs >= a
          -> False

          -- Application of a lambda in the store is not wnf.
          | Just (u, _xs)    <- takeXConApps xx
          , UPrim (NameLoc l) _ <- u
          , Just SLams{}     <- lookupBind l store
          -> False

          -- Application of a data constructor to enough args is not wnf.
          | Just (u, xs)     <- takeXConApps xx
          , and $ map (isWnf store) xs
          , UPrim n _        <- u
          , Just a           <- arityOfName n
          , length xs >= a   
          -> False

          -- Application of some other expression, 
          --  maybe an under-applied primop or data constructor.
          | otherwise   
          -> isWnf store x1 && isWnf store x2


-- | Get the region witness corresponding to one of the witness types that are
--   permitted in a letregion.
regionWitnessOfType :: Type n -> Maybe (Witness n)
regionWitnessOfType tt
 = case tt of
        TApp (TCon (TyConWitness TwConGlobal)) r
         -> Just $ WApp (WCon WiConGlobal)   (WType r)

        TApp (TCon (TyConWitness TwConMutable)) r 
         -> Just $ WApp (WCon WiConMutable)  (WType r)
        
        TApp (TCon (TyConWitness TwConConst)) r
         -> Just $ WApp (WCon WiConConst)    (WType r)

        TApp (TCon (TyConWitness TwConLazy)) r
         -> Just $ WApp (WCon WiConLazy)     (WType r)

        TApp (TCon (TyConWitness TwConManifest)) r
         -> Just $ WApp (WCon WiConManifest) (WType r)

        _ -> Nothing

