
-- | Lowering of deBruijn indices in a type.
---
--   TODO: merge this code with LiftT.
module DDC.Type.Transform.LowerT
        ( lowerT
        , LowerT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as Sum

-- | Lower type indices in a thing the given number of levels.       
lowerT :: (Ord n, LowerT c)
       => Int          -- ^ Number of levels to lower.
       -> c n          -- ^ Lower type indices in this thing.
       -> c n
        
lowerT n xx  = lowerAtDepthT n 0 xx
 


class LowerT (c :: * -> *) where

 -- | Lower type indices that are at least a certain depth by the given number of levels.
 lowerAtDepthT   
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lower.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lower type indices in this thing.
        -> c n
 

instance LowerT Bind where
 lowerAtDepthT n d bb
  = replaceTypeOfBind (lowerAtDepthT n d $ typeOfBind bb) bb
  

instance LowerT Bound where
 lowerAtDepthT n d uu
  = case uu of
        UName{}         -> uu
        UPrim{}         -> uu
        UHole{}         -> uu
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

