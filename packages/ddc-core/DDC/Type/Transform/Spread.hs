
module DDC.Type.Transform.Spread
        (Spread(..))
where

-- | Spread type annotations from the environment and binders,
--   into variables at the leaves.
class Spread (c :: * -> *) where
        spread :: forall n. Env n -> c n -> c n
        
instance Spread Type where
 spread tt
  = case tt of
        TVar u          -> TVar $ spread env u
        TCon tc         -> TCon $ spread env tc
        TForall u t     -> TForall u $ spread (Env.extend u env) t
        TApp t1 t2      -> TApp (spread env t1) (spread env t2)
        TSum ts         -> TSum ...
        TBot k          -> TBot (spread env k)
        
