
-- | Lifting of deBruijn indices in a type.
---
--   TODO: merge this code with LowerT
module DDC.Type.Transform.LiftT
        ( liftT
        , LiftT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as Sum

-- | Lift type indices in a thing the given number of levels.       
liftT  :: (Ord n, LiftT c)
       => Int          -- ^ Number of levels to lift
       -> c n          -- ^ Lift type indices in this thing.
       -> c n
        
liftT n xx  = liftAtDepthT n 0 xx



class LiftT (c :: * -> *) where

 -- | Lift type indices that are at least a certain depth by the given number of levels.
 liftAtDepthT   
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift type indices in this thing.
        -> c n
 
 
instance LiftT Bind where
 liftAtDepthT n d bb
  = replaceTypeOfBind (liftAtDepthT n d $ typeOfBind bb) bb
  

instance LiftT Bound where
 liftAtDepthT n d uu
  = case uu of
        UName{}         -> uu
        UPrim{}         -> uu
        UHole{}         -> uu
        UIx i 
         | d <= i       -> UIx (i + n)
         | otherwise    -> uu

instance LiftT Type where
 liftAtDepthT n d tt
  = let down = liftAtDepthT n
    in case tt of
        TVar u          -> TVar    (down d u)
        TCon{}          -> tt
        TForall b t     -> TForall (down d b)  (down (d + 1) t)
        TApp t1 t2      -> TApp    (down d t1) (down d t2)
        TSum ss         -> TSum    (down d ss)


instance LiftT TypeSum where
 liftAtDepthT n d ss
  = Sum.fromList (liftAtDepthT n d $ Sum.kindOfSum ss)
        $ map (liftAtDepthT n d)
        $ Sum.toList ss

