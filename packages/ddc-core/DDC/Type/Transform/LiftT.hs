
-- | Lifting of type variable indices.
module DDC.Type.Transform.LiftT
        (LiftT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as Sum
import Data.List


class LiftT (c :: * -> *) where
 -- | Lift type indices that are at least a certain depth by the given number of levels.
 liftAtDepthT   
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift type indices in this thing.
        -> c n
 
 -- | Wrapper for `liftAtDepthT` that starts at depth 0.       
 liftT  :: forall n. Ord n
        => Int          -- ^ Number of levels to lift
        -> c n          -- ^ Lift type indices in this thing.
        -> c n
        
 liftT n xx  = liftAtDepthT n 0 xx
 

instance LiftT Bind where
 liftAtDepthT n d bb
  = replaceTypeOfBind (liftAtDepthT n d $ typeOfBind bb) bb
  

instance LiftT Bound where
 liftAtDepthT n d uu
  = case uu of
        UName{}         -> uu
        UIx i t 
         | d <= i       -> UIx (i + n) t
         | otherwise    -> uu
         

instance LiftT Type where
 liftAtDepthT n d tt
  = case tt of
        TVar uu         -> TVar $ liftAtDepthT n d uu
        TCon{}          -> tt
        TForall b t     -> liftAtDepthT n (d + 1) t
        TApp t1 t2      -> TApp (liftAtDepthT n d t1) (liftAtDepthT n d t2)
        TSum ss         -> TSum $ liftAtDepthT n d ss
        TBot k          -> TBot $ liftAtDepthT n d k


instance LiftT TypeSum where
 liftAtDepthT n d ss
  = Sum.fromList (liftAtDepthT n d $ Sum.kindOfSum ss)
        $ map (liftAtDepthT n d)
        $ Sum.toList ss

