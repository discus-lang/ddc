
-- | Single step evalation for the Disciple Core language.
module DDC.Core.Eval.Step 
        ( StepResult(..)
        , force
        , step
        , isValue
        , isWeakValue)
where
import DDC.Core.Eval.Profile
import DDC.Core.Eval.Store
import DDC.Core.Eval.Name
import DDC.Core.Eval.Prim
import DDC.Core.Eval.Env
import DDC.Core.Eval.Compounds
import DDC.Core.Transform.SubstituteWX
import DDC.Core.Transform.SubstituteXX
import DDC.Core.Transform.SubstituteTX
import DDC.Core.Check
import DDC.Core.Fragment.Profile
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Type.Compounds
import qualified Data.Set               as Set


-- StepResult -----------------------------------------------------------------
-- | The result of stepping some expression.
data StepResult
        -- | Expression progressed to a new state.
        = StepProgress Store (Exp () Name)

        -- | Expression cannot step and is a (weak) value.
        --   We're done already.
        | StepDone


        -- | Expression cannot step, and is not a (weak) value.
        --   The original expression was mistyped,
        --   or something is wrong with the interpreter.
        | StepStuck

        -- | Expression is stuck, and we know for sure it's mistyped.
        | StepStuckMistyped (Error () Name)
        deriving (Show)


-- force ----------------------------------------------------------------------
-- | Single step a core expression to a value.
--
--   As opposed to `step`, if the provided expression is the location of a
--   Thunk, then the thunk will be forced.
--
force   :: Store        -- ^ Current store.
        -> Exp () Name  -- ^ Expression to force.
        -> StepResult   -- ^ Result of forcing it.

force store xx
        | (casts, xx')                  <- unwrapCasts xx
        , XCon _ dc                     <- xx'
        , Just (NameLoc l)              <- takeNameOfDaCon dc
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
-- | Single step a code expression to a weak value.
--
--   As opposed to `force`, if the provided expression is the location of a 
--   Thunk, then the thunk is not forced.
--
step    :: Store        -- ^ Current store.
        -> Exp () Name  -- ^ Expression to step.
        -> StepResult   -- ^ Result of stepping it.


-- (EvLam)
-- Add abstractions to the heap.
-- We need the type of the expression to attach to the location
-- This fakes the store typing from the formal typing rules.
step store xx
 | (casts, xp)  <- unwrapCasts xx
 , isLambdaX xp
 = case typeOfExp 
                (configOfProfile  evalProfile) 
                (profilePrimKinds evalProfile)
                (profilePrimTypes evalProfile)
                xp 
   of   Left err -> StepStuckMistyped err
        Right t   
         -> let Just (bs, xBody) = takeXLamFlags xp
                (store', l)      = allocBind (Rgn 0) t (SLams bs xBody) store
                x'               = xLoc l t
            in  StepProgress store' (wrapCasts casts x')


-- (EvAlloc)
-- Construct some data in the heap.
step store xx
        | Just (dc, xs)  <- takeXConApps xx
        , case takeNameOfDaCon dc of
            -- Unit constructors are not allocated into the store.
            Just NamePrimCon{}  -> True
            Just NameInt{}      -> True
            Just NameCon{}      -> True
            _                   -> False

        , Just n                <- takeNameOfDaCon dc
        , Just arity            <- arityOfName n
        , length xs == arity
        , and $ map (isWeakValue store) xs
        , Just (store', x')     <- stepPrimCon dc xs store
        = StepProgress store' x'


-- (EvPrim)
-- Step a primitive operator or constructor defined by the client.
step store xx
        | Just (x1@(XVar _ (UPrim p _)), xs)  <- takeXApps xx
        , Just arity                          <- arityOfName p
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
step store xx
        | Just (x1, xsArgs)   <- takeXApps xx
        , Just l1             <- takeLocX x1
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
                 -> StepProgress store2 (makeXApps () x1 xsArgs')


-- (EvAppSubst)
-- Substitute wnf arguments into an abstraction.
step store xx
        | Just (x1, xsArgs)     <- takeXApps xx
        , (casts, xL1)          <- unwrapCasts x1
        , Just l1               <- takeLocX xL1

        , Just (Rgn 0, _, SLams fbs xBody) <- lookupRegionTypeBind l1 store

        -- Take as many wnfs as possible to satisfy binders.
        , arity                 <- length fbs
        , (wnfs, nonWnfs)       <- span (isWeakValue store) xsArgs

        -- If we have any wnfs at all, then we can do a substitution.
        , not $ null wnfs

        = let argsToSubst     = take arity wnfs
              argsOverApplied = drop (length argsToSubst) wnfs
              argsLeftover    = argsOverApplied ++ nonWnfs
                
              bs              = map snd fbs
              bsToSubst       = take (length argsToSubst) bs
              bsLeftover      = drop (length bsToSubst)   fbs

              xResult         = substituteXArgs (zip bsToSubst argsToSubst)
                              $ makeXLamFlags () bsLeftover xBody

          in  StepProgress store 
                $ wrapCasts casts (makeXApps () xResult argsLeftover)


-- (EvApp1 / EvApp2)
-- Evaluate the components of an application.
step store (XApp a x1 x2)
 | (casts, x1p)         <- unwrapCasts x1
 = case force store x1p of
    StepProgress store' x1p' 
     -> StepProgress store' (XApp a (wrapCasts casts x1p') x2)

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
     -> StepProgress store (substituteXX b x1 x2)

    err -> err


-- (EvLetLazyAlloc)
-- Allocate a lazy binding in the heap.
step store (XLet _ (LLet (LetLazy _w) b x1) x2)
        -- We need the type of the expression to attach to the location
        -- This fakes the store typing from the formal typing rules.
 = case typeOfExp (configOfProfile  evalProfile)
                  (profilePrimKinds evalProfile)
                  (profilePrimTypes evalProfile)
                  x1 
   of
        Left err -> StepStuckMistyped err
        Right t1
         -> let (store1, l)   = allocBind (Rgn 0) t1 (SThunk x1) store
                x1'           = xLoc l t1
            in  StepProgress store1 (substituteXX b x1' x2)


-- (EvLetRec)
-- Add recursive bindings to the store.
step store (XLet _ (LRec bxs) x2)
 = let  (bs, xs) = unzip bxs
        ts       = map typeOfBind bs

        -- Allocate new locations in the store to hold the expressions.
        (store1, ls)  = newLocs (length bs) store
        xls           = [xLoc l t | (l, t) <- zip ls ts]

        -- Substitute locations into all the bindings.
        xs'      = map (substituteXXs (zip bs xls)) xs

        -- Create store objects for each of the bindings.
        mos      = map (\x -> case takeXLamFlags x of
                               Just (fbs', xBody) -> Just $ SLams fbs' xBody
                               _                  -> Nothing)
                       xs'

      -- If this fails then some of the bindings did not have lambdas out the
      -- front. We don't support plain value recursion yet.
   in case sequence mos of
       Nothing        -> StepStuck
       Just os
        -> let -- Add all the objects to the store.
               store2   = foldr (\(l, t, o) -> addBind l (Rgn 0) t o) store1
                        $ zip3 ls ts os
        
               -- Substitute locations into the body expression.
               x2'      = substituteXXs (zip bs xls) x2

           in  StepProgress store2 x2'


-- (EvCreateRegion)
-- Create a new region.
step store (XLet a (LLetRegion bRegion bws) x)
        | Just uRegion  <- takeSubstBoundOfBind bRegion

        -- Allocate a new region handle for the bound region.
        , (store1, uHandle@(UPrim (NameRgn rgn) _))
                        <- primNewRegion store
        , tHandle       <- TCon $ TyConBound uHandle kRegion

        -- Substitute handle into the witness types.
        , bws'          <- map (substituteBoundTX uRegion tHandle) bws

        -- Build witnesses for each of the witness types.
        -- This can fail if the set of witness signatures is malformed.
        , Just wits     <- sequence 
                        $  map regionWitnessOfType
                        $  map typeOfBind bws'

        = let   -- Substitute handle and witnesses into body.
                x'      = substituteBoundTX  uRegion tHandle
                        $ substituteWXs (zip bws' wits)  x

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
     , Just (SObj dc lsArgs)    <- lookupBind lDiscrim store
     , Just nTag                <- takeNameOfDaCon dc
     , Just tsArgs              <- sequence $ map (\l -> lookupTypeOfLoc l store) lsArgs
     , AAlt pat xBody : _       <- filter (tagMatchesAlt nTag) alts
     -> case pat of
           PDefault         
            -> StepProgress store xBody

           PData _ bsArgs      
            | bxsArgs   <- [ (b, wrapCasts casts (xLoc l t))
                                | l     <- lsArgs
                                | t     <- tsArgs
                                | b     <- bsArgs]
            -> StepProgress store
                    (substituteXXs bxsArgs xBody)

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
--   Values can't be progressed any further, with either `force` or `step`.
isValue :: Store -> Exp a Name -> Bool
isValue store xx
        = isSomeValue False store xx


-- | Check if an expression is a weak values.
--   These are all the values, and locations that point to thunks.
--
--   Weak values can be progressed with `force`, but not `step`.
isWeakValue :: Store -> Exp a Name -> Bool
isWeakValue store xx
        = isSomeValue True store xx



-- | Check if an expression is a weak value.
isSomeValue :: Bool -> Store -> Exp a Name -> Bool
isSomeValue weak store xx
 = case xx of
         XVar{}         -> True

         XCon _ dc
          | Just (NameLoc l)    <- takeNameOfDaCon dc
          , Just SThunk{}       <- lookupBind l store
          -> weak

         XCon{}         -> True

         -- Plain lambdas aren't weak values because we always add them
         -- to the store. The resulting store location is then a value.
         XLAM{}         -> False                        
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
          | Just (n, xs)        <- takeXPrimApps xx
          , and $ map (isSomeValue weak store) xs
          , Just a              <- arityOfName n
          , length xs >= a
          -> False

          -- Application of a lambda in the store is not wnf.
          | Just (dc, _xs)      <- takeXConApps xx
          , Just (NameLoc l)    <- takeNameOfDaCon dc
          , Just SLams{}        <- lookupBind l store
          -> False

          -- Application of a data constructor to enough args is not wnf.
          | Just (dc, xs)       <- takeXConApps xx
          , and $ map (isSomeValue weak store) xs
          , Just n              <- takeNameOfDaCon dc
          , Just a              <- arityOfName n
          , length xs >= a   
          -> False

          -- Application of some other expression, 
          --  maybe an under-applied primop or data constructor.
          | otherwise   
          -> isSomeValue weak store x1 
          && isSomeValue weak store x2


-- | Get the region witness corresponding to one of the witness types that are
--   permitted in a letregion.
regionWitnessOfType :: Type Name -> Maybe (Witness Name)
regionWitnessOfType tt
 = case tt of
        TApp (TCon (TyConWitness TwConGlobal))   r -> Just $ wGlobal   r
        TApp (TCon (TyConWitness TwConMutable))  r -> Just $ wMutable  r
        TApp (TCon (TyConWitness TwConConst))    r -> Just $ wConst    r
        TApp (TCon (TyConWitness TwConLazy))     r -> Just $ wLazy     r
        TApp (TCon (TyConWitness TwConManifest)) r -> Just $ wManifest r
        TApp (TApp (TCon (TyConWitness TwConDistinct)) r1) r2
         -> Just $ wDistinct r1 r2
        _                                          -> Nothing

