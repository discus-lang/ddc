-- | Check the kind of a type.
module DDC.Type.Check
        ( Config        (..)
        , configOfProfile

          -- * Kinds of Types
        , checkType
        , checkTypeWithContext
        , kindOfType

          -- * Kinds of Constructors
        , takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon
        
          -- * Errors
        , Error(..))
where
import DDC.Type.DataDef
import DDC.Type.Check.Context
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
import DDC.Control.Monad.Check           (throw, evalCheck)
import qualified DDC.Control.Monad.Check as G
import qualified DDC.Type.Sum            as TS
import qualified DDC.Type.Env            as Env
import qualified Data.Map                as Map

-- | The type checker monad.
type CheckM n   = G.CheckM () (Error n)


-- Wrappers -------------------------------------------------------------------
-- | Check a type in the given environment,
--   returning an error or its kind.
checkType  :: (Ord n, Show n, Pretty n) 
           => Config n 
           -> KindEnv n 
           -> Type n
           -> Either (Error n) (Type n, Kind n)

checkType defs env tt 
        = evalCheck () $ checkTypeM defs env emptyContext tt


-- | Check a type in the given environment and local context,
--   returning an error or its kind.
checkTypeWithContext 
        :: (Ord n, Show n, Pretty n) 
        => Config n 
        -> KindEnv n 
        -> Context n
        -> Type n
        -> Either (Error n) (Type n, Kind n)

checkTypeWithContext defs env ctx tt 
        = evalCheck () $ checkTypeM defs env ctx tt


-- | Check a type in an empty environment, returning an error or its kind.
kindOfType
        :: (Ord n, Show n, Pretty n) 
        => Config n
        -> Type n 
        -> Either (Error n) (Kind n)

kindOfType defs tt
 = liftM snd $ evalCheck () $ checkTypeM defs Env.empty emptyContext tt


-- checkType ------------------------------------------------------------------
-- | Check a type, returning its kind.
---
--   Note that when comparing kinds, we can just use plain equality
--   (==) instead of equivT. This is because kinds do not contain quantifiers
--   that need to be compared up to alpha-equivalence, nor do they contain
--   crushable components terms.
checkTypeM 
        :: (Ord n, Show n, Pretty n) 
        => Config  n
        -> KindEnv n
        -> Context n   
        -> Type n 
        -> CheckM n (Type n, Kind n)

checkTypeM config env ctx tt
        = -- trace (renderPlain $ text "checkTypeM:" <+> text (show tt)) $
          {-# SCC checkTypeM #-}
          checkTypeM' config env ctx tt

-- Variables ------------------
checkTypeM' config env ctx tt@(TVar u)
 -- Primitive type variable has its type directly attached.
 | UPrim _ k    <- u    
 = return (tt, k)

 -- Kind is in the local environment.
 | Just k       <- lookupKind u ctx
 = return (tt, k)
 
 -- Kind is in the global environment.
 | Just k       <- Env.lookup u env
 = return (tt, k)

 -- This was some type we were supposed to infer.
 | UName n      <- u
 , Just isHole  <- configNameIsHole config
 , isHole n
 = throw $ ErrorCannotInfer tt

 -- Type variable is nowhere to be found.
 | otherwise
 = throw $ ErrorUndefined u


-- Constructors ---------------
checkTypeM' config env _ctx tt@(TCon tc)
 = case tc of
        -- Sorts don't have a higher classification.
        TyConSort _      -> throw $ ErrorNakedSort tt

        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        TyConKind kc
         -> case takeSortOfKiCon kc of
                Just s   -> return (tt, s)
                Nothing  -> throw $ ErrorUnappliedKindFun

        TyConWitness tcw -> return (tt, kindOfTwCon tcw)
        TyConSpec    tcc -> return (tt, kindOfTcCon tcc)

        -- User defined type constructors need to be in the set of data defs.
        TyConBound u k
         -> case u of
                UName n
                 | Just def <- Map.lookup n 
                            $  dataDefsTypes $ configDataDefs config
                 -> let k'   = kindOfDataType def
                    in  return (TCon (TyConBound u k'), k')

                 | Just s <- Env.lookupName n
                          $  configPrimSupers config
                 -> return (tt, s)

                 | Just s <- Env.lookupName n env
                 -> return (tt, s)

                 | otherwise
                 -> throw $ ErrorUndefinedTypeCtor u

                UPrim{} -> return (tt, k)
                UIx{}   -> throw $ ErrorUndefinedTypeCtor u

        TyConExists _ k -> return (tt, k)

-- Quantifiers ----------------
checkTypeM' config env ctx tt@(TForall b1 t2)
 = do   let ctx'  = pushKind b1 ctx
        _         <- checkTypeM config env ctx  (typeOfBind b1)
        (t2', k2) <- checkTypeM config env ctx' t2

        -- The body must have data or witness kind.
        when (  (not $ isDataKind k2)
             && (not $ isWitnessKind k2))
         $ throw $ ErrorForallKindInvalid tt t2 k2

        return (TForall b1 t2', k2)


-- Applications ---------------
-- Applications of the kind function constructor are handled directly
-- because the constructor doesn't have a sort by itself.
checkTypeM' config env ctx tt@(TApp (TApp (TCon (TyConKind KiConFun)) k1) k2)
 = do   _       <- checkTypeM config env ctx k1
        (_, s2) <- checkTypeM config env ctx k2
        return  (tt, s2)

-- The implication constructor is overloaded and can have the
-- following kinds:
--   (=>) :: @ ~> @ ~> @,  for witness implication.
--   (=>) :: @ ~> * ~> *,  for a context.
checkTypeM' config env ctx tt@(TApp (TApp tC@(TCon (TyConWitness TwConImpl)) t1) t2)
 = do   (t1', k1) <- checkTypeM config env ctx t1
        (t2', k2) <- checkTypeM config env ctx t2

        let tt' = TApp (TApp tC t1') t2'

        if      isWitnessKind k1 && isWitnessKind k2
         then     return (tt', kWitness)
        else if isWitnessKind k1 && isDataKind k2
         then     return (tt', kData)
        else    throw $ ErrorWitnessImplInvalid tt t1 k1 t2 k2

-- Type application.
checkTypeM' config env ctx tt@(TApp t1 t2)
 = do   (t1', k1)       <- checkTypeM config env ctx t1
        (t2', k2)       <- checkTypeM config env ctx t2
        case k1 of
         TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
          | k11 == k2   -> return (TApp t1' t2', k12)
          | otherwise   -> throw $ ErrorAppArgMismatch tt k1 k2
                  
         _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2

-- Sums -----------------------
checkTypeM' config env ctx (TSum ts)
 = do   (ts', ks)       <- liftM unzip 
                        $  mapM (checkTypeM config env ctx) $ TS.toList ts

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
         then return (TSum (TS.fromList k ts'), k)
         else throw $ ErrorSumKindInvalid ts k

