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

-- Variables ------------------
checkTypeM config env ctx0 uni tt@(TVar u) mode

 -- Kind holes.
 --   This is some kind that we were explicitly told to infer,
 --   so make a new existential for it.
 | UniverseKind         <- uni
 , UName n              <- u
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
 , UName n              <- u
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


 -- A variable in the local context.
 | UniverseSpec          <- uni
 , Just (kActual, _role) <- lookupKind u ctx0
 = case mode of
        Check kExpect
          -> do ctx1    <- makeEq config ctx0 kActual kExpect
                        $  ErrorMismatch uni kActual kExpect tt
                return (tt, kActual, ctx1)

        _ ->    return (tt, kActual, ctx0)

 -- A variable in the global environment.
 | UniverseSpec         <- uni
 , Just kActual         <- Env.lookup u env
 = case mode of
        Check kExpect
          -> do ctx1    <- makeEq config ctx0 kActual kExpect
                        $  ErrorMismatch uni kActual kExpect tt
                return (tt, kActual, ctx1)

        _ ->    return (tt, kActual, ctx0)



 -- A primitive variable with its kind directly attached, but where the
 -- variable is not also in the kind environment. This is a hack used
 -- for static used for static region variables in the evaluator.
 -- We make them constructors rather than variables so that we don't need
 -- to have a data constructor definition for each one.
 | UPrim _ kActual      <- u
 , UniverseSpec         <- uni
 = case mode of
        Check kExpect 
          -> do ctx1    <- makeEq config ctx0 kActual kExpect
                        $  ErrorMismatch uni kActual kExpect tt
                return (tt, kActual, ctx1)

        _ ->    return (tt, kActual, ctx0)


 -- Type variable is nowhere to be found.
 | otherwise
 = throw $ ErrorUndefined u


-- Constructors ---------------
checkTypeM config env ctx0 uni tt@(TCon tc) mode
 = let  
        -- Get the actual kind of the constructor, according to the 
        -- constructor definition.
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
             -- data defs.
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
        (tt', tActual) <- getActual
        case mode of
         -- If we have an expected kind then make the actual kind the same.
         Check tExpect
           -> do ctx1   <- makeEq config ctx0 tActual tExpect
                        $  ErrorMismatch uni tActual tExpect tt
                 return (tt', tActual, ctx1)

         -- In Recon and Synth mode just return the actual kind.
         _ ->    return (tt', tActual, ctx0)


-- Quantifiers ----------------
checkTypeM config env ctx0 UniverseSpec 
        tt@(TForall b1 t2) _mode
 = do   
        -- Check the binder has a valid kind.
        -- Kinds don't have any variables, which is enforced by the fact that
        -- the Var rule above only applies in the Spec universe.
        -- The returned context will be the same as the provided one.
        let t1          = typeOfBind b1
        _               <- checkTypeM config env ctx0 UniverseKind t1 Recon

        -- Check the body with the binder in scope.
        let (ctx1, pos1) = markContext ctx0
        let ctx2         = pushKind b1 RoleAbstract ctx1
        (t2', k2, ctx3) <- checkTypeM config env ctx2 UniverseSpec t2 Recon
        let ctx_cut      = popToPos pos1 ctx3

        -- The body must have data or witness kind.
        let k2'         = applyContext ctx3 k2
        when (  (not $ isDataKind k2')
             && (not $ isWitnessKind k2'))
         $ throw $ ErrorForallKindInvalid tt t2 k2'

        return (TForall b1 t2', k2', ctx_cut)


-- Applications ---------------
-- Applications of the kind function constructor are handled directly
-- because the constructor doesn't have a sort by itself.
checkTypeM config env ctx UniverseKind 
        tt@(TApp (TApp (TCon (TyConKind KiConFun)) k1) k2)
        _mode
 = do   
        -- Kinds don't have any variables.
        -- The returned context will be the same as the provided one.
        _          <- checkTypeM config env ctx UniverseKind k1 Recon
        (_, s2, _) <- checkTypeM config env ctx UniverseKind k2 Recon

        return  (tt, s2, ctx)

-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness implication.
--   (=>) :: @ ~> * ~> *,  for a context.
checkTypeM config env ctx0 UniverseSpec 
        tt@(TApp (TApp tC@(TCon (TyConWitness TwConImpl)) t1) t2)
        _mode
 = do   
        (t1', k1, ctx1) <- checkTypeM config env ctx0 UniverseSpec t1 Recon
        (t2', k2, ctx2) <- checkTypeM config env ctx1 UniverseSpec t2 Recon

        let tt' = TApp (TApp tC t1') t2'

        if      isWitnessKind k1 && isWitnessKind k2
         then     return (tt', kWitness, ctx2)
        else if isWitnessKind k1 && isDataKind k2
         then     return (tt', kData, ctx2)
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

-- Type application.
checkTypeM config env ctx0 UniverseSpec 
        tt@(TApp t1 t2) _mode
 = do   
        (t1', k1, ctx1) <- checkTypeM config env ctx0 UniverseSpec t1 Recon
        (t2', k2, ctx2) <- checkTypeM config env ctx1 UniverseSpec t2 Recon

        case k1 of
         -- Type constructor application.
         -- The constructor must have the function kind and its parameter
         -- kind match with the kind of the argument.
         --
         -- TODO: don't unify with existentials in Recon mode because
         -- the caller may not expect the context to change. Say that we 
         -- never solve constraints in Recon mode.
         --
         TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
          -> do ctx3    <- makeEq config ctx2 k11 k2
                        $  ErrorAppArgMismatch tt k1 k2
                return  (TApp t1' t2', k12, ctx3)
                  
         _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2


-- Sums -----------------------
checkTypeM config kenv ctx0 UniverseSpec (TSum ts) _mode
 = do   
        -- Check all the types, chaining the context from left to right.
        (ts', ks, ctx1) 
                <- checkTypesM config kenv ctx0 UniverseSpec Recon
                $  TS.toList ts

        -- Check that all the types in the sum have the same kind.
        let kExpect = TS.kindOfSum ts
        k'      <- case nub ks of     
                     []     -> return $ TS.kindOfSum ts
                     [k]    -> return k
                     _      -> throw $ ErrorSumKindMismatch kExpect ts ks
        
        -- Check that the kind of the elements is a valid one.
        -- Only effects and closures can be summed.
        if (k' == kEffect || k' == kClosure)
         then return (TSum (TS.fromList k' ts'), k', ctx1)
         else throw $ ErrorSumKindInvalid ts k'


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
 = do   (t',  k',  ctx1)  <- checkTypeM  config kenv ctx0 uni t Recon
        (ts', ks', ctx')  <- checkTypesM config kenv ctx1 uni mode ts
        return  (t' : ts', k' : ks', ctx')

