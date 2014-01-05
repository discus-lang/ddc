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
--   NOTE: The type/kinder isn't bidirectional because we haven't thought
--   of a reason to make it so. We don't have quantifiers at the kind level,
--   so and don't have subkinding, so don't need the full power of the exp/type
--   checker. Making it bidirectional would allow us to infer the missing kind
--   in types like  (/\(a : ?). \(x : Int). x), when used in a context with an 
--   expected type which constraints the kind of 'a' ... but handling situations
--   like this isn't a good enough reason.
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

 -- Look in the local context.
 | UniverseSpec         <- uni
 , Just (k, _role)      <- lookupKind u ctx0
 = case mode of
        Check kExpect
          -> do ctx1    <- makeEq (ErrorMismatch uni kExpect k tt) 
                                ctx0 k kExpect
                return (tt, k, ctx1)

        _ ->    return (tt, k, ctx0)

 -- Look in the global environment.
 | UniverseSpec         <- uni
 , Just k               <- Env.lookup u env
 = case mode of
        Check kExpect
          -> do ctx1    <- makeEq (ErrorMismatch uni kExpect k tt) 
                                ctx0 k kExpect
                return (tt, k, ctx1)

        _ ->    return (tt, k, ctx0)

 -- A primitive type variable with its kind directly attached, but is not in
 -- the kind environment. This is a hack used for static used for static
 -- region variables in the evaluator.
 -- TODO: Why aren't these constructors instead of variables?
 | UPrim _ k            <- u
 , UniverseSpec         <- uni
 = case mode of
        Check kExpect 
          -> do ctx1    <- makeEq (ErrorMismatch uni kExpect k tt)
                                ctx0 k kExpect
                return (tt, k, ctx1)

        _ ->    return (tt, k, ctx0)

 -- Type holes.
 -- This is some type or kind that we are supposed to infer.
 --
 -- TODO: Also handle holes in the spec universe, 
 --       but we will need to infer the kind as well in synth mode.
 --       Allocate two existentials at once.
 | UName n      <- u
 , Just isHole  <- configNameIsHole config
 , isHole n
 , UniverseKind <- uni          
 = case mode of
        -- We don't infer kind holes in recon mode.
        -- We should have been given a program with complete kind annotations.
        Recon
          -> do  throw $ ErrorUndefined u

        -- With bidirectional checking, make a new existential.
        -- TODO: if we were given an expected sort then use that instead of
        --       forcing inferred kinds to the Comp sort.
        _ -> do
                i       <- newExists
                let t    =  typeOfExists i
                let ctx' =  pushExists i ctx0
                return  (t, sComp, ctx')

 -- Type variable is nowhere to be found.
 | otherwise
 = throw $ ErrorUndefined u


-- Constructors ---------------
checkTypeM config env ctx0 uni tt@(TCon tc) mode
 = let  getActual
         -- Sorts don't have a higher classification.
         | TyConSort _           <- tc
         , UniverseSort          <- uni
         = throw $ ErrorNakedSort tt

         -- Can't sort check a naked kind function
         -- because the sort depends on the argument kinds.
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
             | Just def <- Map.lookup n 
                        $  dataDefsTypes $ configDataDefs config
             , UniverseSpec <- uni
             -> let k'   = kindOfDataType def
                in  return (TCon (TyConBound u k'), k')

             -- The kinds of abstract imported type constructors are in the
             -- kind environment.
             | Just s   <- Env.lookupName n env
             , UniverseSpec <- uni
             -> return (tt, s)

             -- We don't have a type for this constructor.
             | otherwise
             -> throw $ ErrorUndefinedTypeCtor u

            -- The kinds of primitive type constructors are directly attached.
            UPrim{} -> return (tt, k)

            -- Type constructors are always defined at top-level and not
            -- by anonymous debruijn binding.
            UIx{}   -> throw $ ErrorUndefinedTypeCtor u

         -- Existentials can be either in the Spec or Kind universe.
         | TyConExists _ k       <- tc
         , uni == UniverseSpec || uni == UniverseKind
         = return (tt, k)

         -- Whatever constructor we were given wasn't in the expected universe.
         | otherwise
         = throw $ ErrorUniverseMalfunction tt uni
 in do
        (tt', kActual) <- getActual
        case mode of
         -- If we have an expected kind then make the actual kind the 
         -- same as it.
         Check kExpect
           -> do ctx1    <- makeEq (ErrorMismatch uni kExpect kActual tt)
                                 ctx0 kExpect kActual
                 return (tt', kActual, ctx1)

         -- In Recon and Synth mode return the actual kind of the constructor.
         _ ->    return (tt', kActual, ctx0)


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
        when (  (not $ isDataKind k2)
             && (not $ isWitnessKind k2))
         $ throw $ ErrorForallKindInvalid tt t2 k2

        return (TForall b1 t2', k2, ctx_cut)


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
          -> do ctx3    <- makeEq (ErrorAppArgMismatch tt k1 k2) ctx2 k11 k2
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

