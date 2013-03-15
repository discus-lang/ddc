-- | Check the kind of a type.
module DDC.Type.Check
        ( Config        (..)
        , configOfProfile

          -- * Kinds of Types
        , checkType
        , kindOfType

          -- * Kinds of Constructors
        , takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon
        
          -- * Errors
        , Error(..))
where
import DDC.Type.DataDef
import DDC.Type.Check.Error
import DDC.Type.Check.ErrorMessage      ()
import DDC.Type.Check.CheckCon
import DDC.Type.Check.Config
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Exp
import DDC.Base.Pretty
import Data.List
import Control.Monad
import DDC.Type.Pretty                   ()
import DDC.Type.Env                      (KindEnv)
import DDC.Control.Monad.Check           (throw, result)
import qualified DDC.Control.Monad.Check as G
import qualified DDC.Type.Sum            as TS
import qualified DDC.Type.Env            as Env
import qualified Data.Map                as Map

-- | The type checker monad.
type CheckM n   = G.CheckM (Error n)



-- Wrappers -------------------------------------------------------------------
-- | Check a type in the given environment, returning an error or its kind.
checkType  :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> KindEnv n 
           -> Type n 
           -> Either (Error n) (Kind n)

checkType defs env tt 
        = result $ checkTypeM defs env tt


-- | Check a type in an empty environment, returning an error or its kind.
kindOfType :: (Ord n, Show n, Pretty n) 
           => Config n
           -> Type n 
           -> Either (Error n) (Kind n)

kindOfType defs tt
        = result $ checkTypeM defs Env.empty tt


-- checkType ------------------------------------------------------------------
-- | Check a type, returning its kind.
---
--   Note that when comparing kinds, we can just use plain equality
--   (==) instead of equivT. This is because kinds do not contain quantifiers
--   that need to be compared up to alpha-equivalence, nor do they contain
--   crushable components terms.
checkTypeM 
        :: (Ord n, Show n, Pretty n) 
        => Config n
        -> KindEnv n
        -> Type n 
        -> CheckM n (Kind n)

checkTypeM config env tt
        = -- trace (renderPlain $ text "checkTypeM:" <+> text (show tt)) $
          {-# SCC checkTypeM #-}
          checkTypeM' config env tt

-- Variables ------------------
checkTypeM' _config env (TVar u)
 = case Env.lookup u env of
        Just t  -> return t
        Nothing -> throw $ ErrorUndefined u

-- Constructors ---------------
checkTypeM' config _env tt@(TCon tc)
 = case tc of
        -- Sorts don't have a higher classification.
        TyConSort _      -> throw $ ErrorNakedSort tt

        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        TyConKind kc
         -> case takeSortOfKiCon kc of
                Just s   -> return s
                Nothing  -> throw $ ErrorUnappliedKindFun

        TyConWitness tcw -> return $ kindOfTwCon tcw
        TyConSpec    tcc -> return $ kindOfTcCon tcc

        -- User defined type constructors need to be in the set of data defs.
        TyConBound u k
         -> case u of
                UName n
                 | Just _ <- Map.lookup n 
                          $  dataDefsTypes $ configPrimDataDefs config
                 -> return k

                 | Just s <- Env.lookupName n
                          $  configPrimSupers config
                 -> return s

                 | otherwise
                 -> throw $ ErrorUndefinedCtor u

                UPrim{} -> return k
                UIx{}   -> throw $ ErrorUndefinedCtor u


-- Quantifiers ----------------
checkTypeM' config env tt@(TForall b1 t2)
 = do   _       <- checkTypeM config env (typeOfBind b1)
        k2      <- checkTypeM config (Env.extend b1 env) t2

        -- The body must have data or witness kind.
        when (  (not $ isDataKind k2)
             && (not $ isWitnessKind k2))
         $ throw $ ErrorForallKindInvalid tt t2 k2

        return k2

-- Applications ---------------
-- Applications of the kind function constructor are handled directly
-- because the constructor doesn't have a sort by itself.
checkTypeM' config env (TApp (TApp (TCon (TyConKind KiConFun)) k1) k2)
 = do   _       <- checkTypeM config env k1
        s2      <- checkTypeM config env k2
        return  s2

-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness implication.
--   (=>) :: @ ~> * ~> *,  for a context.
checkTypeM' config env tt@(TApp (TApp (TCon (TyConWitness TwConImpl)) t1) t2)
 = do   k1      <- checkTypeM config env t1
        k2      <- checkTypeM config env t2
        if      isWitnessKind k1 && isWitnessKind k2
         then     return kWitness
        else if isWitnessKind k1 && isDataKind k2
         then     return kData
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

-- Type application.
checkTypeM' config env tt@(TApp t1 t2)
 = do   k1      <- checkTypeM config env t1
        k2      <- checkTypeM config env t2
        case k1 of
         TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
          | k11 == k2   -> return k12
          | otherwise   -> throw $ ErrorAppArgMismatch tt k1 k2
                  
         _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2

-- Sums -----------------------
checkTypeM' config env (TSum ts)
 = do   ks      <- mapM (checkTypeM config env) $ TS.toList ts

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
         then return k
         else throw $ ErrorSumKindInvalid ts k

