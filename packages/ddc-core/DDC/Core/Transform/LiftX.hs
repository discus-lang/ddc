
-- | Lifting of type variable indices.
module DDC.Core.Transform.LiftX
        (LiftX(..))
where
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Universe


class LiftX (c :: * -> *) where
 -- | Lift exp indices that are at least a certain depth by the given number of levels.
 liftAtDepthX
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift exp indices in this thing.
        -> c n
 
 -- | Wrapper for `liftAtDepthX` that starts at depth 0.       
 liftX  :: forall n. Ord n
        => Int          -- ^ Number of levels to lift
        -> c n          -- ^ Lift exp indices in this thing.
        -> c n
        
 liftX n xx  = liftAtDepthX n 0 xx
  

instance LiftX Bound where
 liftAtDepthX n d uu
  = case uu of
        UName{}         -> uu
        UPrim{}         -> uu
        UIx i t 
         | d <= i       -> UIx (i + n) t
         | otherwise    -> uu


instance LiftX (Exp a) where
 liftAtDepthX n d xx
  = let down = liftAtDepthX n d
    in case xx of
        XVar a u        -> XVar a (down u)
        XCon{}          -> xx
        XApp a x1 x2    -> XApp a (down x1) (down x2)

        XLam a b x
         | Just UniverseComp <- universeFromType1 (typeOfBind b)
         -> XLam a b (liftAtDepthX n (d + 1) x)
         
         | otherwise 
         -> XLam a b (down x)
         
        XLet{}          -> error "liftX XLet not done yet"
        XCase{}         -> error "liftX XCase not done yet"

        XCast a cc x    -> XCast a cc (down x)
        
        XType{}         -> xx
        XWitness{}      -> xx
         
        
        
        
