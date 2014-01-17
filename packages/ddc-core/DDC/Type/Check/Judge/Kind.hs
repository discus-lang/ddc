module DDC.Type.Check.Judge.Kind
        (checkTypeM)
where
import DDC.Type.DataDef
import DDC.Type.Check.Context
import DDC.Type.Check.Error
import DDC.Type.Check.ErrorMessage      ()
import DDC.Type.Check.CheckCon
import DDC.Type.Check.Config
import DDC.Type.Check.Base
import DDC.Type.Check.Judge.Eq
import DDC.Type.Universe
import Data.List
import Control.Monad
import DDC.Type.Pretty                   ()
import DDC.Type.Env                      (KindEnv)
import qualified DDC.Type.Sum            as TS
import qualified DDC.Type.Env            as Env
import qualified Data.Map                as Map


-- | Check a type returning its kind, or a kind returning its sort.
--
--   The unverse of the thing to check is directly specified, and if the 
--   thing is not actually in this universe they you'll get an error.
--
--   We track what universe the provided kind is in for defence against
--   transform bugs. Types like ([a : [b : Data]. b]. a -> a), should not be
--   accepted by the source parser, but may be created by bogus program
--   transformations. Quantifiers cannot be used at the kind level, so it's 
--   better to fail early.
---
--   Note that when comparing kinds, we can just use plain equality
--   (==) instead of equivT. This is because kinds do not contain quantifiers
--   that need to be compared up to alpha-equivalence, nor do they contain
--   crushable components terms.
--
checkTypeM 
        :: (Ord n, Show n, Pretty n) 
        => Config n             -- ^ Type checker configuration.
        -> KindEnv n            -- ^ Top-level kind environment.
        -> Context n            -- ^ Local context.
        -> Universe             -- ^ What universe the type to check is in.
        -> Type n               -- ^ The type to check (can be a Spec or Kind)
        -> Mode n               -- ^ Type checker mode.
        -> CheckM n 
                ( Type n
                , Kind n
                , Context n)

-- Variables ------------------------------------
checkTypeM config env ctx0 uni tt@(TVar u) mode

 -- Kind holes.
 --   This is some kind that we were explicitly told to infer,
 --   so make a new existential for it.
 | UniverseKind         <- uni
 , Just n               <- takeNameOfBound u
 , Just isHole          <- configNameIsHole config
 , isHole n
 = case mode of
        -- We don't infer kind holes in recon mode.
        -- The program should have complete kind annotations.
        Recon
         -> do  throw $ ErrorUndefined u

        -- Synthesised kinds are assumed to have sort Comp.
        Synth
         -> do  i        <- newExists sComp
                let t    = typeOfExists i
                let ctx' = pushExists i ctx0
                return (t, sComp, ctx')

        -- We have an expected sort for the existential,
        -- so use that.
        Check sExpected
         -> do  i        <- newExists sExpected
                let t    = typeOfExists i
                let ctx' = pushExists i ctx0
                return (t, sExpected, ctx')

 -- Spec holes.
 --   This is some spec that we were explicitly told to infer,
 --   so make an existential for it.
 | UniverseSpec         <- uni
 , Just n               <- takeNameOfBound u
 , Just isHole          <- configNameIsHole config
 , isHole n
 = case mode of
        -- We don't infer spec holes in recon mode.
        -- The program should have complete spec annotations.
        Recon
         -> do  throw $ ErrorUndefined u

        -- Synthesised types could have an arbitrary kind, 
        -- so we need to make two existentials.
        Synth
         -> do  iK       <- newExists sComp
                let k    =  typeOfExists iK
                let ctx1 =  pushExists iK ctx0

                iT       <- newExists k
                let t    =  typeOfExists iT
                let ctx2 =  pushExists iT ctx1

                return  (t, k, ctx2)

        -- We have an expected kind for the existential,
        -- so use that.
        Check kExpected
         -> do  iT       <- newExists kExpected
                let t    =  typeOfExists iT
                let ctx1 =  pushExists iT ctx0
                return  (t, kExpected, ctx1)


 -- Some variable defined in an environment, 
 -- or a primitive variable with its kind directly attached.
 | UniverseSpec          <- uni
 = let  
        -- Get the actual kind of the variable,
        -- according to the kind environment.
        getActual
         -- A variable in the local environment.
         | Just (k, _role) <- lookupKind u ctx0
         = return k

         -- A variable in the global environment.
         | Just k          <- Env.lookup u env
         = return k

         -- A primitive type variable with its kind directly attached, but where
         -- the variable is not also in the kind environment. This is a hack used
         -- for static used for static region variables in the evaluator.
         -- We make them constructors rather than variables so that we don't need
         -- to have a data constructor definition for each one.
         | UPrim _ k       <- u
         = return k

         -- Type variable is no where to be found.
         | otherwise
         = throw $ ErrorUndefined u

   in do
        kActual <- getActual
        let kActual'    = applyContext ctx0 kActual

        -- In Check mode we check the expected kind against the actual
        -- kind from the environment.
        case mode of
         Check kExpected
          -> do 
                let kExpected'  = applyContext ctx0 kExpected
                ctx1    <- makeEq config ctx0 kActual' kExpected'
                        $  ErrorMismatch uni  kActual' kExpected' tt

                return (tt, kActual', ctx1)
 
         _ ->   return (tt, kActual', ctx0)

 -- Type variables are not a part of this universe.
 | otherwise
 = throw $ ErrorUniverseMalfunction tt uni


-- Constructors ---------------------------------
checkTypeM config env ctx0 uni tt@(TCon tc) mode
 = let  
        -- Get the actual kind of the constructor, 
        -- according to the constructor definition.
        getActual
         -- Sort constructors don't have a higher classification.
         -- We should never try to check these.
         | TyConSort _           <- tc
         , UniverseSort          <- uni
         = throw $ ErrorNakedSort tt

         -- Baked-in kind constructors.
         -- We can't sort-check a naked kind function constructor because
         -- the sort of a fully applied one depends on the argument kind.
         | TyConKind kc          <- tc
         , UniverseKind          <- uni
         = case takeSortOfKiCon kc of
                Just s   -> return (tt, s)
                Nothing  -> throw $ ErrorUnappliedKindFun

         -- Baked-in witness type constructors.
         | TyConWitness tcw      <- tc
         , UniverseSpec          <- uni
         = return (tt, kindOfTwCon tcw)

         -- Baked-in spec type constructors.
         | TyConSpec    tcc      <- tc
         , UniverseSpec          <- uni
         = return (tt, kindOfTcCon tcc)

         -- Fragment specific, or user defined constructors.
         | TyConBound u k        <- tc
         = case u of
            UName n
             -- User defined data type constructors must be in the set of
             -- data defs. Attach the real kind why we're here.
             | Just def         <- Map.lookup n 
                                $  dataDefsTypes $ configDataDefs config
             , UniverseSpec     <- uni
             -> let k'   = kindOfDataType def
                in  return (TCon (TyConBound u k'), k')

             -- The kinds of abstract imported type constructors are in the
             -- global kind environment.
             | Just s           <- Env.lookupName n env
             , UniverseSpec     <- uni
             -> return (tt, s)

             -- We don't have a type for this constructor.
             | otherwise
             -> throw $ ErrorUndefinedTypeCtor u

            -- The kinds of primitive type constructors are directly attached.
            UPrim{} -> return (tt, k)

            -- Type constructors are always defined at top-level and not
            -- by anonymous debruijn binding.
            UIx{}   -> throw $ ErrorUndefinedTypeCtor u

         -- Existentials can be either in the Spec or Kind universe,
         -- and their kinds/sorts are directly attached.
         | TyConExists _ t       <- tc
         , uni == UniverseSpec || uni == UniverseKind
         = return (tt, t)

         -- Whatever constructor we were given wasn't in the expected universe.
         | otherwise
         = throw $ ErrorUniverseMalfunction tt uni
 in do
        -- Get the actual kind/sort of the constructor according to the 
        -- constructor definition.
        (tt', kActual)  <- getActual
        let kActual'    =  applyContext ctx0 kActual

        case mode of
         -- If we have an expected kind then make the actual kind the same.
         Check kExpected
           -> do 
                 let kExpected' = applyContext ctx0 kExpected
                 ctx1   <- makeEq config ctx0 kActual' kExpected'
                        $  ErrorMismatch uni  kActual' kExpected' tt
                 return (tt', kActual', ctx1)

         -- In Recon and Synth mode just return the actual kind.
         _ ->    return (tt', kActual', ctx0)


-- Quantifiers ----------------------------------
checkTypeM config kenv ctx0 uni@UniverseSpec 
        tt@(TForall b1 t2) mode
 = case mode of
    Recon
     -> do
        -- Check the binder is well sorted.
        let t1  = typeOfBind b1
        _       <- checkTypeM config kenv ctx0 UniverseKind t1 Recon

        -- Check the body with the binder in scope.
        let (ctx1, pos1) = markContext ctx0
        let ctx2         = pushKind b1 RoleAbstract ctx1
        (t2', k2, ctx3) <- checkTypeM config kenv ctx2 UniverseSpec t2 Recon

        -- The body must have kind Data or Witness.
        let k2'         = applyContext ctx3 k2
        when ( not (isDataKind k2')
            && not (isWitnessKind k2'))
         $ throw $ ErrorForallKindInvalid tt t2 k2'

        -- Pop the quantified type off the context.
        let ctx_cut      = popToPos pos1 ctx3

        return (TForall b1 t2', k2', ctx_cut)

    Synth
     -> do
        -- Synthesise a sort for the binder.
        let k1  = typeOfBind b1
        (k1', _s1, ctx1) <- checkTypeM config kenv ctx0 UniverseKind k1 Synth
                
        let b1' = replaceTypeOfBind k1' b1

        -- Check the body with the binder in scope.
        let (ctx2, pos1) = markContext ctx1
        let ctx3         = pushKind b1' RoleAbstract ctx2
        (t2', k2, ctx4) <- checkTypeM config kenv ctx3 UniverseSpec t2 Synth

        -- If the kind of the body is unconstrained then default it to Data.
        -- See [Note: Defaulting the kind of quantified types]
        let k2' = applyContext ctx4 k2
        (k2'', ctx5)
         <- if isTExists k2'
             then do
                ctx5    <- makeEq config ctx4 k2' kData
                        $  ErrorMismatch uni  k2' kData tt
                return (applyContext ctx5 k2', ctx5)

             else do
                return (k2', ctx4)

        -- The above horror show needs to have worked.
        when ( not (isDataKind k2'')
            && not (isWitnessKind k2''))
         $ throw $ ErrorForallKindInvalid tt t2 k2''

        -- Pop the quantified type off the context.
        let ctx_cut      = popToPos pos1 ctx5

        return (TForall b1' t2', k2'', ctx_cut)

    Check kExpected 
     -> do
        -- Synthesise a sort for the binder.
        let k1  = typeOfBind b1
        (k1', _s1, ctx1) <- checkTypeM config kenv ctx0 UniverseKind k1 Synth

        let b1' = replaceTypeOfBind k1' b1

        -- Check the body with the binder in scope.
        let (ctx2, pos1) = markContext ctx1
        let ctx3         = pushKind b1' RoleAbstract ctx2
        (t2', k2, ctx4) <- checkTypeM config kenv ctx3 UniverseSpec t2 Synth

        -- In Check mode if *both* the current kind of the body and the expected
        -- kind are existentials then force them both to be data. Otherwise make
        -- the kind of the body the same as the expected kind.
        -- See [Note: Defaulting the kind of quantified types]
        let k2' = applyContext ctx4 k2
        (k2'', ctx5)
         <- if isTExists k2' && isTExists kExpected
             then do
                ctx'    <- makeEq config ctx4 k2' kExpected
                        $  ErrorMismatch uni  k2' kExpected tt

                ctx5    <- makeEq config ctx' k2' kData 
                        $  ErrorMismatch uni  k2' kData  tt
                return (applyContext ctx5 k2', ctx5)

             else do
                ctx5    <- makeEq config ctx4 k2' kExpected
                        $  ErrorMismatch uni  k2' kExpected tt
                return (applyContext ctx5 k2', ctx4)

        -- The above horror show needs to have worked.
        when ( not (isDataKind k2'')
            && not (isWitnessKind k2''))
         $ throw $ ErrorForallKindInvalid tt t2 k2'

        -- Pop the quantified type off the context.
        let ctx_cut      = popToPos pos1 ctx5

        return (TForall b1 t2', k2'', ctx_cut)


-- Applications ---------------------------------
-- Applications of the kind function constructor are handled directly
-- because the constructor doesn't have a sort by itself.
-- The sort of a kind function is the sort of the result.
checkTypeM config kenv ctx0 uni@UniverseKind 
        tt@(TApp (TApp (TCon (TyConKind KiConFun)) k1) k2) mode
 = case mode of
    Recon
     -> do
        _               <- checkTypeM config kenv ctx0 uni k1 Recon
        (_, s2, _)      <- checkTypeM config kenv ctx0 uni k2 Recon
        return  (tt, s2, ctx0)

    Synth
     -> do
        (k1',  _, ctx1) <- checkTypeM config kenv ctx0 uni k1 Synth
        (k2', s2, ctx2) <- checkTypeM config kenv ctx1 uni k2 Synth
        return  (kFun k1' k2', s2, ctx2)

    Check sExpected
     -> do
        (k1',  _, ctx1) <- checkTypeM config kenv ctx0 uni k1 Synth
        (k2', s2, ctx2) <- checkTypeM config kenv ctx1 uni k2 (Check sExpected)
        return  (kFun k1' k2', s2, ctx2)


-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness constructors.
--   (=>) :: @ ~> * ~> *,  for functions that take witnesses.
checkTypeM config env ctx0 uni@UniverseSpec 
        tt@(TApp (TApp tC@(TCon (TyConWitness TwConImpl)) t1) t2) mode
 = case mode of
    Recon
     -> do
        (t1', k1, ctx1) <- checkTypeM config env ctx0 uni t1 Recon
        (t2', k2, ctx2) <- checkTypeM config env ctx1 uni t2 Recon

        let tt' = TApp (TApp tC t1') t2'

        if      isWitnessKind k1 && isWitnessKind k2
         then     return (tt', kWitness, ctx2)
        else if isWitnessKind k1 && isDataKind k2
         then     return (tt', kData, ctx2)
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

    Synth
     -> do
        (t1', _k1, ctx1) <- checkTypeM config env ctx0 uni t1 Synth
        (t2', k2,  ctx2) <- checkTypeM config env ctx1 uni t2 Synth

        return (tImpl t1' t2', k2, ctx2)

    Check kExpected
     -> do
        (t1', _k1, ctx1) <- checkTypeM config env ctx0 uni t1 Synth
        (t2', k2,  ctx2) <- checkTypeM config env ctx1 uni t2 (Check kExpected)

        return (tImpl t1' t2', k2, ctx2)


-- General type application.
checkTypeM config kenv ctx0 UniverseSpec 
        tt@(TApp tFn tArg) mode
 = case mode of
    Recon
     -> do
        -- Check the kind of the functional part.
        (tFn',  kFn,  ctx1) 
         <- checkTypeM config kenv ctx0 UniverseSpec tFn Recon
        
        -- Check the kind of the argument.
        (tArg', kArg, ctx2) 
         <- checkTypeM config kenv ctx1 UniverseSpec tArg Recon

        -- The kind of the parameter must match that of the argument
        case kFn of
         TApp (TApp (TCon (TyConKind KiConFun)) kParam) kBody
           |  equivT kParam kArg
           -> return (tApp tFn' tArg', kBody, ctx2)

           | otherwise
           -> throw $ ErrorAppArgMismatch tt tFn' kFn tArg' kArg

         _ -> throw $ ErrorAppNotFun tt tFn' kFn tArg'

    Synth
     -> do
        -- Synthesise a kind for the functional part.
        (tFn', kFn, ctx1) 
         <- checkTypeM config kenv ctx0 UniverseSpec tFn Synth

        -- Apply the argument to the function.
        (kResult, tArg', ctx2)
         <- synthTAppArg config kenv ctx1
                tFn' (applyContext ctx1 kFn )
                tArg

        return (TApp tFn' tArg', kResult, ctx2)

    Check kExpected
     -> do
        -- Synthesise a kind for the overall type.
        (t1', k1, ctx1) 
         <- checkTypeM config kenv ctx0 UniverseSpec tt Synth

        -- Force the synthesised kind to be the same as the expected one.
        let k1'         = applyContext ctx1 k1
        let kExpected'  = applyContext ctx1 kExpected
        ctx2    <- makeEq config ctx1         k1' kExpected'
                $  ErrorMismatch UniverseKind k1' kExpected' tt

        return (t1', k1', ctx2)


-- Sums -----------------------------------------
checkTypeM config kenv ctx0 UniverseSpec tt@(TSum ss) mode
 = case mode of
    Recon
     -> do   
        -- Check all the elements,
        --  threading the context from left to right.
        (ts', ks, ctx1) 
                <- checkTypesM config kenv ctx0 UniverseSpec Recon
                $  TS.toList ss

        -- Check that all the types in the sum have the same kind.
        let kExpect = TS.kindOfSum ss
        k'      <- case nub ks of     
                     []     -> return $ TS.kindOfSum ss
                     [k]    -> return k
                     _      -> throw $ ErrorSumKindMismatch kExpect ss ks

        -- Check that the kind of the elements is a valid one.
        -- Only effects and closures can be summed.
        if (k' == kEffect || k' == kClosure)
         then return (TSum (TS.fromList k' ts'), k', ctx1)
         else throw $ ErrorSumKindInvalid ss k'

    Synth
     -> do
        -- Synthesise a kind for all the elements,
        --  threading the context from left to right.
        (ts, ks, ctx1)
                <- checkTypesM config kenv ctx0 UniverseSpec Synth
                $  TS.toList ss

        case ks of
         -- Force all elements to have the same kind as the first one.
         -- Note that (TS.kindOfSum ts) will be Bot in an unannotated program,
         -- so we can't use that directly.
         k : _ksMore
          -> do 
                (ts'', _, ctx2)
                 <- checkTypesM config kenv ctx1 UniverseSpec (Check k) ts

                let k'  = applyContext ctx2 k
                return  (TSum (TS.fromList k' ts''), k', ctx2)

         -- If the sum contains no elements then we don't know what kind
         -- it's supposed to be. This shouldn't happen in a well formed program,
         -- so just default it to the Effect kind.
         [] ->  return  (TSum (TS.fromList kEffect []), kEffect, ctx0)

    Check kExpected
     -> do
        -- Synthesise a kind for the overall type.
        (t1', k1, ctx1)
                <- checkTypeM config kenv ctx0 UniverseSpec tt Synth

        -- Force the synthesised kind to match the expected one.
        let k1'         = applyContext ctx1 k1
        let kExpected'  = applyContext ctx1 kExpected
        ctx2    <- makeEq config ctx1         k1' kExpected'
                $  ErrorMismatch UniverseKind k1' kExpected' tt

        return  (t1', k1, ctx2)


-- Whatever type we were given wasn't in the specified universe.
checkTypeM _ _ _ uni tt _mode
        = throw $ ErrorUniverseMalfunction tt uni


-------------------------------------------------------------------------------
-- | Like `checkTypeM` but do several, chaining the contexts appropriately.
checkTypesM 
        :: (Ord n, Show n, Pretty n) 
        => Config n             -- ^ Type checker configuration.
        -> KindEnv n            -- ^ Top-level kind environment.
        -> Context n            -- ^ Local context.
        -> Universe             -- ^ What universe the types to check are in.
        -> Mode n               -- ^ Type checker mode.
        -> [Type n]             -- ^ The types to check.
        -> CheckM n 
                ( [Type n]
                , [Kind n]
                , Context n)

checkTypesM _ _ ctx0 _ _ []
 = return ([], [], ctx0)

checkTypesM config kenv ctx0 uni mode (t : ts)
 = do   (t',  k',  ctx1)  <- checkTypeM  config kenv ctx0 uni t mode
        (ts', ks', ctx')  <- checkTypesM config kenv ctx1 uni mode ts
        return  (t' : ts', k' : ks', ctx')


-------------------------------------------------------------------------------
-- | Synthesise the type of a kind function applied to its argument.
synthTAppArg
        :: (Show n, Ord n, Pretty n)
        => Config n
        -> KindEnv n
        -> Context n
        -> Type n               -- Type function.
        -> Kind n               -- Kind of functional part.
        -> Type n               -- Type argument.
        -> CheckM n
                ( Kind n        -- Kind of result.
                , Type n        -- Checked type argument.
                , Context n)    -- Result context. 

synthTAppArg config kenv ctx0 tFn kFn tArg

 | Just iFn     <- takeExists kFn
 = do  
        -- New existential for the kind of the function parameter.
        iParam          <- newExists sComp
        let kParam      = typeOfExists iParam

        -- New existential for the kind of the function body
        iBody           <- newExists sComp
        let kBody       = typeOfExists iBody

        -- Update the context with the new constraint.
        let Just ctx1   = updateExists [iBody, iParam] iFn 
                                (kFun kParam kBody) ctx0

        -- Check the argument under the new context.
        (tArg', _kArg, ctx2)
         <- checkTypeM config kenv ctx1 UniverseSpec tArg (Check kParam)

        return (kBody, tArg', ctx2)


 | TApp (TApp (TCon (TyConKind KiConFun)) kParam) kBody <- kFn
 = do   
        -- The kind of the argument must match the parameter kind
        (tArg', _kArg, ctx1) 
         <- checkTypeM config kenv ctx0 UniverseSpec tArg (Check kParam)

        return (kBody, tArg', ctx1)

 | otherwise
 = throw $ ErrorAppNotFun (TApp tFn tArg) tFn kFn tArg 


-- [Note: Defaulting the kind of quantified types]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- For expressions like:
--   /\a. \(x : [b : Data]. a). ()
--  
-- The kind of 'a' must be Data because 'x' is used as a parameter of a function 
-- abstraction. If the kind of the body of a quantified type is unconstrained 
-- then we default it to data.
--
-- Although the types of witness constructors have quantified types, 
-- those types are primitive, so we never need to do type inference for them.
-- There aren't any cases where defaulting the kind of a quantified type to 
-- Data would be the wrong thing to do.
--
