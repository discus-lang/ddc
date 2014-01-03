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


-- | Check a type, returning its kind.
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

checkTypeM config env ctx uni tt
        = {-# SCC checkTypeM #-}
          checkTypeM' config env ctx uni tt

-- Variables ------------------
checkTypeM' config env ctx UniverseSpec tt@(TVar u)

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
 -- the kind environment. This is a hack used for static used for static region
 -- variables in the evaluator.
 -- TODO: Why aren't these constructors instead of variables?
 | UPrim _ k    <- u
 = return (tt, k, ctx)

 -- Type variable is nowhere to be found.
 | otherwise
 = throw $ ErrorUndefined u


-- Constructors ---------------
checkTypeM' config env ctx _uni tt@(TCon tc)
 = case tc of
        -- Sorts don't have a higher classification.
        TyConSort _      -> throw $ ErrorNakedSort tt

        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        TyConKind kc
         -> case takeSortOfKiCon kc of
                Just s   -> return (tt, s, ctx)
                Nothing  -> throw $ ErrorUnappliedKindFun

        TyConWitness tcw -> return (tt, kindOfTwCon tcw, ctx)
        TyConSpec    tcc -> return (tt, kindOfTcCon tcc, ctx)

        -- User defined type constructors need to be in the set of data defs.
        TyConBound u k
         -> case u of
                UName n
                 | Just def <- Map.lookup n 
                            $  dataDefsTypes $ configDataDefs config
                 -> let k'   = kindOfDataType def
                    in  return (TCon (TyConBound u k'), k', ctx)

                 | Just s <- Env.lookupName n
                          $  configPrimSupers config
                 -> return (tt, s, ctx)

                 | Just s <- Env.lookupName n env
                 -> return (tt, s, ctx)

                 | otherwise
                 -> throw $ ErrorUndefinedTypeCtor u

                UPrim{} -> return (tt, k, ctx)
                UIx{}   -> throw $ ErrorUndefinedTypeCtor u

        TyConExists _ k -> return (tt, k, ctx)


-- Quantifiers ----------------
checkTypeM' config env ctx UniverseSpec tt@(TForall b1 t2)
 = do   
        -- Check the binder has a valid kind.
        -- Kinds don't have any variables, so we don't need to do anything
        -- with the returned context.
        _               <- checkTypeM config env ctx UniverseSpec (typeOfBind b1)

        -- Check the body with the binder in scope.
        let (ctx1, pos1) = markContext ctx
        let ctx2         = pushKind b1 RoleAbstract ctx1
        (t2', k2, ctx3)  <- checkTypeM config env ctx2 UniverseSpec t2

        let ctx_cut     = popToPos pos1 ctx3

        -- The body must have data or witness kind.
        when (  (not $ isDataKind k2)
             && (not $ isWitnessKind k2))
         $ throw $ ErrorForallKindInvalid tt t2 k2

        return (TForall b1 t2', k2, ctx_cut)


-- Applications ---------------
-- Applications of the kind function constructor are handled directly
-- because the constructor doesn't have a sort by itself.
checkTypeM' config env ctx UniverseKind 
        tt@(TApp (TApp (TCon (TyConKind KiConFun)) k1) k2)
 = do   _          <- checkTypeM config env ctx UniverseKind k1
        (_, s2, _) <- checkTypeM config env ctx UniverseKind k2
        return  (tt, s2, ctx)

-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness implication.
--   (=>) :: @ ~> * ~> *,  for a context.
checkTypeM' config env ctx UniverseSpec 
        tt@(TApp (TApp tC@(TCon (TyConWitness TwConImpl)) t1) t2)
 = do   (t1', k1, ctx1) <- checkTypeM config env ctx  UniverseSpec t1
        (t2', k2, ctx2) <- checkTypeM config env ctx1 UniverseSpec t2

        let tt' = TApp (TApp tC t1') t2'

        if      isWitnessKind k1 && isWitnessKind k2
         then     return (tt', kWitness, ctx2)
        else if isWitnessKind k1 && isDataKind k2
         then     return (tt', kData, ctx2)
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

-- Type application.
checkTypeM' config env ctx uni tt@(TApp t1 t2)
 = do   (t1', k1, ctx1) <- checkTypeM config env ctx  uni t1
        (t2', k2, ctx2) <- checkTypeM config env ctx1 uni t2
        case k1 of
         TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
          | k11 == k2   -> return (TApp t1' t2', k12, ctx2)
          | otherwise   -> throw $ ErrorAppArgMismatch tt k1 k2
                  
         _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2


-- Sums -----------------------
checkTypeM' config env ctx UniverseSpec (TSum ts)
 = do   
        -- TODO: handle contexts properly.
        (ts', ks, _)    <- liftM unzip3 
                        $  mapM (checkTypeM config env ctx UniverseSpec) 
                        $  TS.toList ts

        -- Check that all the types in the sum have a single kind, 
        -- and return that kind.
        k <- case nub ks of     
                 []     -> return $ TS.kindOfSum ts
                 [k]    -> return k
                 _      -> throw $ ErrorSumKindMismatch 
                                        (TS.kindOfSum ts) ts ks
        
        -- Check that the kind of the elements is a valid one.
        -- Only effects and closures can be summed.
        if (k == kEffect || k == kClosure)
         then return (TSum (TS.fromList k ts'), k, ctx)
         else throw $ ErrorSumKindInvalid ts k

checkTypeM' _ _ _ _ _
        = error "checkTypeM': Universe malfunction"                             -- TODO: real error
