
-- | Lifting of type variable indices.
--   TODO: merge this code with LiftT.
module DDC.Type.Transform.LowerT
        (LowerT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as Sum


class LowerT (c :: * -> *) where
 -- | Lower type indices that are at least a certain depth by the given number of levels.
 lowerAtDepthT   
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lower.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lower type indices in this thing.
        -> c n
 
 -- | Wrapper for `liftAtDepthT` that starts at depth 0.       
 lowerT :: forall n. Ord n
        => Int          -- ^ Number of levels to lower.
        -> c n          -- ^ Lower type indices in this thing.
        -> c n
        
 lowerT n xx  = lowerAtDepthT n 0 xx
 

instance LowerT Bind where
 lowerAtDepthT n d bb
  = replaceTypeOfBind (lowerAtDepthT n d $ typeOfBind bb) bb
  

instance LowerT Bound where
 lowerAtDepthT n d uu
  = case uu of
        UName{}         -> uu
        UPrim{}         -> uu
        UIx i t 
         | d <= i       -> UIx (i - n) t
         | otherwise    -> uu
         

instance LowerT Type where
 lowerAtDepthT n d tt
  = let down = lowerAtDepthT n 
    in case tt of
        TVar uu         -> TVar    (down d uu)
        TCon{}          -> tt
        TForall b t     -> TForall (down d b)  (down (d + 1) t)
        TApp t1 t2      -> TApp    (down d t1) (down d t2)
        TSum ss         -> TSum    (down d ss)


instance LowerT TypeSum where
 lowerAtDepthT n d ss
  = Sum.fromList (lowerAtDepthT n d $ Sum.kindOfSum ss)
        $ map (lowerAtDepthT n d)
        $ Sum.toList ss

