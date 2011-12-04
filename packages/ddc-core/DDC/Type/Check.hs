
module DDC.Type.Check
        ( -- * Kinds of Types
          checkType
        , kindOfType
        , kindOfType'

          -- * Kinds of Constructors
        , sortOfKiCon
        , kindOfTwCon
        , kindOfTcCon
        
          -- * Errors
        , Error(..))
where
import DDC.Type.Check.CheckError
import DDC.Type.Check.CheckCon
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Base.Pretty
import Data.List
import DDC.Type.Check.Monad             (throw, result)
import DDC.Type.Pretty                  ()
import DDC.Type.Env                     (Env)
import qualified DDC.Type.Sum           as TS
import qualified DDC.Type.Env           as Env
import qualified DDC.Type.Check.Monad   as G


type CheckM n   = G.CheckM (Error n)


-- Wrappers ---------------------------------------------------------------------------------------
-- | Check a type in the given environment, returning an error or its kind.
checkType :: Ord n => Env n -> Type n -> Either (Error n) (Kind n)
checkType env tt = result $ checkTypeM env tt


-- | Check a type in an empty environment, returning an error or its kind.
kindOfType  :: Ord n => Type n -> Either (Error n) (Kind n)
kindOfType tt = result $ checkTypeM Env.empty tt


-- | Check a type in an empty environment, returning its kind, or `error` if there isn't one.
kindOfType' :: (Ord n, Pretty n) => Type n -> Kind n
kindOfType' tt
 = case kindOfType tt of
        Left err        -> error $ show $ (ppr err)
        Right k         -> k


-- checkType --------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
--   TODO: check that existing annotations have the same kinds as from the environment.
--         add a function to check that a type has kind annots in the right places.
checkTypeM :: Ord n => Env n -> Type n -> CheckM n (Kind n)
checkTypeM env tt
 = case tt of
        -- Constructors ---------------
        -- Sorts don't have a higher classification.
        TCon (TyConSort _)
         -> throw $ ErrorNakedSort tt
 
        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        TCon (TyConKind kc)
         -> case sortOfKiCon kc of
                Just s  -> return s
                Nothing -> throw $ ErrorUnappliedKindFun

        TCon (TyConWitness tc)
         -> return $ kindOfTwCon tc

        TCon (TyConComp tc)
         -> return $ kindOfTcCon tc


        -- Variables ------------------
        TVar uu
         -> case Env.lookup uu env of
                Nothing -> throw  $ ErrorUndefined uu
                Just k  -> return k 
        
        
        -- Quantifiers ----------------
        TForall b1 t2
         -> do  _       <- checkTypeM env (typeOfBind b1)
                checkTypeM (Env.extend b1 env) t2
        

        -- Applications ---------------
        -- Applications of the kind function constructor are handled directly because
        -- the constructor doesn't have a sort by itself.
        TApp (TApp (TCon (TyConKind KiConFun)) k1) k2
         -> do  _       <- checkTypeM env k1
                s2      <- checkTypeM env k2
                return  s2

        -- TODO: need to use type equiv judgement intead
        --       beacuse           Pure (e1 + e2)
        --       won't match with  Pure (e2 + e1)
        --       no, this will be fine when we move to type sums
        TApp t1 t2
         -> do  k1      <- checkTypeM env t1
                k2      <- checkTypeM env t2
                case k1 of
                 TApp (TApp (TCon (TyConKind KiConFun)) k11) k12
                  | k11 == k2   -> return k12
                  | otherwise   -> throw $ ErrorAppArgMismatch tt k11 k2
                  
                 _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2


        -- Sums -----------------------
        TSum ts
         -> do  ks      <- mapM (checkTypeM env) $ TS.toList ts

                -- Check that all the types in the sum have a single kind, 
                -- and return that kind.
                k <- case nub ks of     
                         []     -> return $ TS.kindOfSum ts
                         [k]    -> return k
                         _      -> (throw $ ErrorSumKindMismatch (TS.kindOfSum ts) ts ks)
                
                -- Check that the kind of the elements is a valid one.
                -- Only effects and closures can be summed.
                if (k == kEffect || k == kClosure)
                 then return k
                 else (throw $ ErrorSumKindInvalid ts k)

