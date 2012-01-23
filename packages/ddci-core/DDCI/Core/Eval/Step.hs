
-- | Single step evalation for the DDC core language.
--
--   These are the rules for the plain calculus. The rules for primitive
--   operators and constructors are defined in the interpreter package, 
--   as they depend on the exact representation of the store.
--
module DDCI.Core.Eval.Step 
        ( step
        , StepResult(..)
        , isValue
        , isWeakValue
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


-- force ----------------------------------------------------------------------
force   :: Store        -- ^ Current store.
        -> Exp () Name  -- ^ Expression to force.
        -> StepResult   -- ^ Result of stepping it.

force store xx
        | (casts, xx')                  <- unwrapCasts xx
        , XCon _ (UPrim (NameLoc l) _)  <- xx'
        , Just (rgn, t, SThunk x)       <- lookupRegionTypeBind l store
        = case force store x of
                StepProgress store' x'
                 -> let store2 = addBind l rgn t (SThunk x') store'
                    in  StepProgress store2 (wrapCasts casts xx)
                
                StepDone 
                 -> StepProgress store x

                err -> err

        | otherwise
        = step store xx


-- step -----------------------------------------------------------------------
-- | Perform a single step reduction of a core expression.
--   This evaluates expressions to a weak value.
step    :: Store        -- ^ Current store.
        -> Exp () Name  -- ^ Expression to step.
        -> StepResult   -- ^ Result of stepping it.


-- (EvLam)
-- Add abstractions to the heap.
step store xx@XLam{}
        -- We need the type of the expression to attach to the location
        -- This fakes the store typing from the formal typing rules.
 = case typeOfExp primDataDefs xx of
        Left err -> StepStuckMistyped err
        Right t   
         -> let Just (bs, xBody)  = takeXLams xx
                (store', l)       = allocBind (Rgn 0) t (SLams bs xBody) store
            in  StepProgress store' (XCon () (UPrim (NameLoc l) t))
        

-- (EvAlloc)
-- Construct some data in the heap.
-- TODO: handle non primitive constructos.
step store xx
        | Just (u, xs)  <- takeXConApps xx
        , case u of
            -- Unit constructors are not allocated into the store.
            UPrim (NamePrimCon PrimDaConUnit) _ -> False
            UPrim NamePrimCon{} _               -> True
            UPrim NameInt{}     _               -> True
            UPrim NameCon{}     _               -> True
            _                                   -> False

        , UPrim n _     <- u
        , Just arity    <- arityOfName n
        , length xs == arity
        , and $ map (isWeakValue store) xs
        , Just (store', x')     <- stepPrimCon n xs store
        = StepProgress store' x'


-- (EvPrim)
-- Step a primitive operator or constructor defined by the client.
step store xx
        | x1@(XVar _ (UPrim p _)) : xs  <- takeXApps xx
        , Just arity                    <- arityOfName p
        = let
                -- TODO: we're not allowing over-applied primops
                stepArg i _acc []
                 | i == arity
                 , Just  (store', x') <- stepPrimOp p xs store
                 = Right (store', x')

                 -- The arguments are all values, but the primop didn't step.
                 | otherwise
                 = Left StepStuck

                stepArg i acc (ax:axs)
                 = case force store ax of
                    StepProgress store' x' 
                     -> Right (store', makeXApps () x1 (reverse acc ++ (x' : axs)))

                    StepDone
                     -> case stepArg (i + 1) (ax : acc) axs of
                         Left  err      -> Left err
                         result         -> result

                    err  -> Left err

          in case stepArg 0 [] xs of
                Left err                -> err
                Right (store', x')      -> StepProgress store' x'



-- (EvAppArgs)
-- Step the left-most non-wnf argument of a lambda.
-- TODO: handle cast wrapped around function.
step store xx
        | xL1 : xsArgs  <- takeXApps xx
        , Just l1       <- takeLocX xL1
        , Just (Rgn 0, _, SLams bs _xBody)  <- lookupRegionTypeBind l1 store

        -- See if an arg to any of the lambdas needs to be stepped.
        , arity         <- length bs
        , wnfs          <- map (isWeakValue store) xsArgs
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
-- TODO: handle cast wrapped around function.
step store xx
        | xL1 : xsArgs  <- takeXApps xx
        , Just l1       <- takeLocX xL1
        , Just (Rgn 0, _, SLams bs xBody) <- lookupRegionTypeBind l1 store

        -- Take as many wnfs as possible to satisfy binders.
        , arity                 <- length bs
        , (wnfs, nonWnfs)       <- span (isWeakValue store) xsArgs

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


-- (EvApp1 / EvApp2)
-- Evaluate the components of an application.
step store (XApp a x1 x2)
 = case step store x1 of
    StepProgress store' x1' 
     -> StepProgress store' (XApp a x1' x2)

    StepDone 
     -> case step store x2 of
         StepProgress store' x2'
          -> StepProgress store' (XApp a x1 x2')
        
         err -> err

    err      -> err


-- (EvLetStrictStep / EvLetStrictSubst)
-- Substitute in a bound value in a let expression.
step store (XLet a (LLet LetStrict b x1) x2)
 = case step store x1 of
    StepProgress store' x1' 
     -> StepProgress store' (XLet a (LLet LetStrict b x1') x2)

    StepDone
     -> StepProgress store (substituteX b x1 x2)

    err -> err


-- (EvLetLazyAlloc)
-- Allocate a lazy binding in the heap.
step store (XLet _ (LLet (LetLazy _w) b x1) x2)
        | t1            <- typeOfBind b
        , (store1, l)   <- allocBind (Rgn 0) t1 (SThunk x1) store
        , x1'           <- XCon () (UPrim (NameLoc l) t1)
        = StepProgress store1 (substituteX b x1' x2)


-- (EvLetRec)
-- Add recursive bindings to the store.
step store (XLet _ (LRec bxs) x2)
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
               store2   = foldr (\(l, t, o) -> addBind l (Rgn 0) t o) store1
                        $ zip3 ls ts os
        
               -- Substitute locations into the body expression.
               x2'      = substituteXs (zip bs xls) x2

           in  StepProgress store2 x2'


-- (EvCreateRegion)
-- Create a new region.
step store (XLet a (LLetRegion bRegion bws) x)
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
step store (XLet _ (LWithRegion r@(UPrim (NameRgn rgn) _)) x)
        | isWeakValue store x
        , Set.member rgn (storeGlobal store)
        = StepProgress store x

        | isWeakValue store x
        , Just store'    <- primDelRegion r store
        = StepProgress store' x

 
-- (EvWithRegion)
-- Reduction within a region context.
step store (XLet a (LWithRegion uRegion) x)
 = case step store x of
    StepProgress store' x' 
          -> StepProgress store' (XLet a (LWithRegion uRegion) x')
    err   -> err


-- (EvCaseStep / EvCaseMatch)
-- Case branching.
step store (XCase a xDiscrim alts)
 = case force store xDiscrim of
    StepProgress store' xDiscrim'
     -> StepProgress store' (XCase a xDiscrim' alts)

    StepDone
     | (casts, xDiscrim')       <- unwrapCasts xDiscrim
     , Just lDiscrim            <- takeLocX xDiscrim'
     , Just (SObj nTag lsArgs)  <- lookupBind lDiscrim store
     , Just tsArgs              <- sequence $ map (\l -> lookupTypeOfLoc l store) lsArgs
     , AAlt pat xBody : _       <- filter (tagMatchesAlt nTag) alts
     -> case pat of
           PDefault         
            -> StepProgress store xBody

           PData _ bsArgs      
            | bxsArgs   <- [ (b, wrapCasts casts (XCon a (UPrim (NameLoc l) t)))
                                | l     <- lsArgs
                                | t     <- tsArgs
                                | b     <- bsArgs]
            -> StepProgress store
                    (substituteXs bxsArgs xBody)

     | otherwise
     -> StepStuck

    err -> err


-- (EvPurifyEject)
-- Eject values from purify casts as there are no more effects to be had.
step store (XCast _ (CastPurify _) x)
        | isWeakValue store x
        = StepProgress store x


-- (EvCast)
-- Evaluate under casts.
step store (XCast a cc x)
 = case step store x of
    StepProgress store' x'
          -> StepProgress store' (XCast a cc x')
    err   -> err


-- (Done/Stuck)
-- Either already a value, or expression is stuck.
step store xx
 | isWeakValue store xx = StepDone
 | otherwise            = StepStuck
        

-- Casts ----------------------------------------------------------------------
-- Unwrap casts from the front of an expression.
unwrapCasts :: Exp () n -> ([Cast n], Exp () n)
unwrapCasts xx
 = case xx of
        XCast _ c x       
         -> let (cs, x') = unwrapCasts x 
            in  (c : cs, x')
        
        _ -> ([], xx)


-- Wrap casts around an expression.
wrapCasts   :: [Cast n] -> Exp () n -> Exp () n
wrapCasts cc xx
 = case cc of
        []      -> xx
        c : cs  -> XCast () c (wrapCasts cs xx)


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

        
-- isValue ----------------------------------------------------------------
-- | Check if an expression is a value.
--   These are all the values, and locations that point to thunks.
isWeakValue :: Store -> Exp a Name -> Bool
isWeakValue store xx
        = isSomeValue True store xx

isValue :: Store -> Exp a Name -> Bool
isValue store xx
        = isSomeValue False store xx


-- | Check if an expression is a weak value.
isSomeValue :: Bool -> Store -> Exp a Name -> Bool
isSomeValue weak store xx
 = case xx of
         XVar{}         -> True

         XCon _ (UPrim (NameLoc l) _)
          | Just SThunk{}       <- lookupBind l store
          -> weak

         XCon{}         -> True

         XLam{}         -> False
         XLet{}         -> False
         XCase{}        -> False

         XCast _ (CastPurify _) _ 
          -> False

         XCast _ _ x    
          -> isSomeValue weak store x

         XType{}        -> True
         XWitness{}     -> True

         XApp _ x1 x2

          -- Application if a primop to enough args is not wnf.
          | Just (n, xs)     <- takeXPrimApps xx
          , and $ map (isSomeValue weak store) xs
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
          , and $ map (isSomeValue weak store) xs
          , UPrim n _        <- u
          , Just a           <- arityOfName n
          , length xs >= a   
          -> False

          -- Application of some other expression, 
          --  maybe an under-applied primop or data constructor.
          | otherwise   
          -> isSomeValue weak store x1 
          && isSomeValue weak store x2


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

