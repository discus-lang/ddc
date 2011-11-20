
module DDC.Type.Check.Exp 
        (kindOfType, kindOfType'
        , Error(..))
where
import DDC.Type.Exp
import DDC.Type.Pretty
import DDC.Type.Compounds
import DDC.Type.Check.Con
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- Wrappers ---------------------------------------------------------------------------------------
-- | Take the kind of a type.
kindOfType  :: Ord n => Type n -> Either (Error n) (Kind n)
kindOfType tt
 = case checkType emptyEnv tt of
         CheckM r       -> r


-- | Take the kind of a type, or `error` if there isn't one.
kindOfType' :: (Ord n, Pretty n) => Type n -> Kind n
kindOfType' tt
 = case kindOfType tt of
        Left err        -> error $ show $ (ppr err)
        Right k         -> k


-- checkType --------------------------------------------------------------------------------------
-- | Check a type, returning its kind.
checkType :: Ord n => Env n -> Type n -> CheckM n (Kind n)
checkType env tt
 = case tt of
        -- Sorts don't have a higher classification.
        TCon (TConSort _)
         -> throw $ ErrorNakedSort tt
 
        -- Can't sort check a naked kind function
        -- because the sort depends on the argument kinds.
        TCon TConKindFun 
         -> throw $ ErrorUnappliedKindFun
          
        TCon (TConKind kc)
         -> return $ sortOfKiCon kc

        TCon (TConType tc)
         -> return $ kindOfTyCon tc

        TVar uu
         -> case lookupEnv uu env of
                Nothing -> throw  $ ErrorUndefined uu
                Just k  -> return k 
        
        TForall b1 t2
         -> do  _       <- checkType env (kindOfBind b1)
                checkType (extendEnv b1 env) t2
        
        -- Applications of the kind function constructor are handled directly because
        -- the constructor doesn't have a sort by itself.
        TApp (TApp (TCon TConKindFun) k1) k2
         -> do  _       <- checkType env k1
                s2      <- checkType env k2
                return  s2
        
        -- TODO: need to use type equiv judgement intead
        --       beacuse           Pure (e1 + e2)
        --       won't match with  Pure (e2 + e1)
        --       no, this will be fine when we move to type sums
        TApp t1 t2
         -> do  k1      <- checkType env t1
                k2      <- checkType env t2
                case k1 of
                 TApp (TApp (TCon TConKindFun) k11) k12
                  | k11 == k2   -> return k12
                  | otherwise   -> throw $ ErrorAppArgMismatch tt k11 k2
                  
                 _              -> throw $ ErrorAppNotFun tt t1 k1 t2 k2

        TSum{} 
         -> error "TSums not done"

        TBot k
         -> return k


-- Check Environment ------------------------------------------------------------------------------
-- | Type environment used when checking.
data Env n
        = Env
        { envMap        :: Map n (Kind n)
        , envStack      :: [Kind n] }


-- | An empty environment.
emptyEnv :: Env n
emptyEnv = Env
        { envMap        = Map.empty
        , envStack      = [] }


-- | Extend an environment with a new binding.
extendEnv :: Ord n => Bind n -> Env n -> Env n
extendEnv bb env
 = case bb of
         BName n k      -> env { envMap   = Map.insert n k (envMap env) }
         BAnon   k      -> env { envStack = k : envStack env }


-- | Lookup a bound variable from an environment.
lookupEnv :: Ord n => Bound n -> Env n -> Maybe (Kind n)
lookupEnv uu env
 = case uu of
        UName n _       -> Map.lookup n (envMap env)
        UIx i _         -> lookup i (zip [0..] (envStack env))


-- Check Monad ------------------------------------------------------------------------------------
-- | Type checking monad.
data CheckM n a
        = CheckM (Either (Error n) a)

instance Monad (CheckM n) where
 return x   = CheckM (Right x)
 (>>=) m f  
  = case m of
          CheckM (Left err)     -> CheckM (Left err)
          CheckM (Right x)      -> f x

          
-- | Throw a type error in the monad.
throw :: Error n -> CheckM n a
throw err       = CheckM $ Left err


-- | Type errors.
data Error n
        = ErrorAppArgMismatch   (Type n) (Kind n) (Kind n)

        | ErrorAppNotFun
        { errorChecking         :: Type n
        , errorFunType          :: Type n
        , errorFunTypeKind      :: Kind n
        , errorArgType          :: Type n
        , errorArgTypeKind      :: Kind n }

        | ErrorUnappliedKindFun 
        | ErrorNakedSort        (Sort n)
        | ErrorUndefined        (Bound n)
        deriving Show

instance Pretty n => Pretty (Error n) where
 ppr err
  = case err of
        ErrorAppArgMismatch tt t1 t2
         -> sep $ punctuate line 
                [ text "Core type mismatch."
                , text "             type: " <> ppr t1
                , text "   does not match: " <> ppr t2
                , text "   in application: " <> ppr tt ]
         
        ErrorAppNotFun tt t1 k1 t2 k2
         -> sep $ punctuate line
                [ text "Core type mismatch."
                , text "     cannot apply type: " <> ppr t2
                , text "               of kind: " <> ppr k2
                , text "  to non-function type: " <> ppr t1
                , text "               of kind: " <> ppr k1
                , text "         in appliction: " <> ppr tt]
                
        ErrorUnappliedKindFun 
         -> text "Can't take sort of unapplied kind function constructor."
        
        ErrorNakedSort s
         -> text "Can't check a naked sort: " <> ppr s

        ErrorUndefined u
         -> text "Undefined variable: " <> ppr u
