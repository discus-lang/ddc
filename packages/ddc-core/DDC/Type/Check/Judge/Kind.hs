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
import DDC.Type.Universe
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Exp
import DDC.Base.Pretty
import Data.List
import Control.Monad
import DDC.Type.Pretty                   ()
import DDC.Type.Env                      (KindEnv)
import DDC.Control.Monad.Check           (throw)
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
        -> CheckM n 
                ( Type n
                , Kind n
                , Context n)

-- Variables ------------------
checkTypeM config env ctx UniverseSpec tt@(TVar u)

 -- Look in the local context.
 | Just (k, _role)      <- lookupKind u ctx
 =      return (tt, k, ctx)

 -- Look in the global environment.
 | Just k               <- Env.lookup u env
 =      return (tt, k, ctx)

 -- This was some type we were supposed to infer.
 -- TODO: For holes at the kind level we also need to infer their sorts.
 | UName n      <- u
 , Just isHole  <- configNameIsHole config
 , isHole n
 = throw $ ErrorCannotInfer tt

 -- A primitive type variable with its kind directly attached, but is not in
 -- the kind environment. This is a hack used for static used for static
 -- region variables in the evaluator.
 -- TODO: Why aren't these constructors instead of variables?
 | UPrim _ k    <- u
 = return (tt, k, ctx)

 -- Type variable is nowhere to be found.
 | otherwise
 = throw $ ErrorUndefined u


-- Constructors ---------------
checkTypeM config env ctx uni tt@(TCon tc)
        -- Sorts don't have a higher classification.
        | TyConSort _           <- tc
        , UniverseSort          <- uni
        = throw $ ErrorNakedSort tt

        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        | TyConKind kc          <- tc
        , UniverseKind          <- uni
        = case takeSortOfKiCon kc of
                Just s   -> return (tt, s, ctx)
                Nothing  -> throw $ ErrorUnappliedKindFun

        -- Baked-in witness type constructors.
        | TyConWitness tcw      <- tc
        , UniverseSpec          <- uni
        = return (tt, kindOfTwCon tcw, ctx)

        -- Baked-in spec type constructors.
        | TyConSpec    tcc      <- tc
        , UniverseSpec          <- uni
        = return (tt, kindOfTcCon tcc, ctx)

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
               in  return (TCon (TyConBound u k'), k', ctx)

            -- The kinds of abstract imported type constructors are in the
            -- kind environment.
            | Just s   <- Env.lookupName n env
            , UniverseSpec <- uni
            -> return (tt, s, ctx)

            -- We don't have a type for this constructor.
            | otherwise
            -> throw $ ErrorUndefinedTypeCtor u

           -- The kinds of primitive type constructors are directly attached.
           UPrim{} -> return (tt, k, ctx)

           -- Type constructors are always defined at top-level and not
           -- by anonymous debruijn binding.
           UIx{}   -> throw $ ErrorUndefinedTypeCtor u

        -- Existentials can be either in the Spec or Kind universe.
        | TyConExists _ k       <- tc
        , uni == UniverseSpec || uni == UniverseKind
        = return (tt, k, ctx)

        -- Whatever constructor we were given wasn't in the expected universe.
        | otherwise
        = throw $ ErrorUniverseMalfunction tt uni


-- Quantifiers ----------------
checkTypeM config env ctx0 UniverseSpec 
        tt@(TForall b1 t2)
 = do   
        -- Check the binder has a valid kind.
        -- Kinds don't have any variables, which is enforced by the fact that
        -- the Var rule above only applies in the Spec universe.
        -- The returned context will be the same as the provided one.
        let t1          = typeOfBind b1
        _               <- checkTypeM config env ctx0  UniverseKind t1

        -- Check the body with the binder in scope.
        let (ctx1, pos1) = markContext ctx0
        let ctx2         = pushKind b1 RoleAbstract ctx1
        (t2', k2, ctx3) <- checkTypeM config env ctx2 UniverseSpec t2
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
 = do   
        -- Kinds don't have any variables.
        -- The returned context will be the same as the provided one.
        _          <- checkTypeM config env ctx UniverseKind k1
        (_, s2, _) <- checkTypeM config env ctx UniverseKind k2

        return  (tt, s2, ctx)

-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness implication.
--   (=>) :: @ ~> * ~> *,  for a context.
checkTypeM config env ctx0 UniverseSpec 
        tt@(TApp (TApp tC@(TCon (TyConWitness TwConImpl)) t1) t2)
 = do   
        (t1', k1, ctx1) <- checkTypeM config env ctx0 UniverseSpec t1
        (t2', k2, ctx2) <- checkTypeM config env ctx1 UniverseSpec t2

        let tt' = TApp (TApp tC t1') t2'

        if      isWitnessKind k1 && isWitnessKind k2
         then     return (tt', kWitness, ctx2)
        else if isWitnessKind k1 && isDataKind k2
         then     return (tt', kData, ctx2)
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

-- Type application.
checkTypeM config env ctx0 UniverseSpec 
        tt@(TApp t1 t2)
 = do   
        (t1', k1, ctx1) <- checkTypeM config env ctx0 UniverseSpec t1
        (t2', k2, ctx2) <- checkTypeM config env ctx1 UniverseSpec t2

        case k1 of
         -- Type constructor application.
         -- The constructor must have the function kind and its parameter
         -- kind match with the kind of the argument.
         TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
          | k11 == k2   -> return (TApp t1' t2', k12, ctx2)

          | otherwise   -> throw $ ErrorAppArgMismatch tt k1 k2
                  
         _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2


-- Sums -----------------------
checkTypeM config kenv ctx0 UniverseSpec (TSum ts)
 = do   
        -- Check all the types, chaining the context from left to right.
        (ts', ks, ctx1) 
                <- checkTypesM config kenv ctx0 UniverseSpec
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
checkTypeM _ _ _ uni tt
        = throw $ ErrorUniverseMalfunction tt uni


-------------------------------------------------------------------------------
-- | Like `checkTypeM` but do several, chaining the contexts appropriately.
checkTypesM 
        :: (Ord n, Show n, Pretty n) 
        => Config n             -- ^ Type checker configuration.
        -> KindEnv n            -- ^ Top-level kind environment.
        -> Context n            -- ^ Local context.
        -> Universe             -- ^ What universe the types to check are in.
        -> [Type n]             -- ^ The types to check.
        -> CheckM n 
                ( [Type n]
                , [Kind n]
                , Context n)

checkTypesM _ _ ctx0 _ []
 = return ([], [], ctx0)

checkTypesM config kenv ctx0 uni (t : ts)
 = do   (t',  k',  ctx1)  <- checkTypeM  config kenv ctx0 uni t
        (ts', ks', ctx')  <- checkTypesM config kenv ctx1 uni ts
        return  (t' : ts', k' : ks', ctx')

