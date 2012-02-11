
-- | Lifting of expression variable indices.
module DDC.Core.Transform.LiftX
        (LiftX(..))
where
import DDC.Core.Exp


class LiftX (c :: * -> *) where
 -- | Lift indices that are at least the given depth by some number
 --   of levels.
 liftAtDepthX
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift expression indices in this thing.
        -> c n
 
 -- | Wrapper for `liftAtDepthX` that starts at depth 0.       
 liftX  :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> c n          -- ^ Lift expression indices in this thing.
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
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLam a b (liftAtDepthX n (d + 1) x)
         
        XLet a lets x   
         -> let (lets', levels) = liftAtDepthXLets n d lets 
            in  XLet a lets' (liftAtDepthX n (d + levels) x)

        XCase a x alts  -> XCase a (down x) (map down alts)
        XCast a cc x    -> XCast a cc (down x)
        XType{}         -> xx
        XWitness{}      -> xx
         

instance LiftX (Alt a) where
 liftAtDepthX n d (AAlt p x)
  = case p of
	PDefault 
         -> AAlt PDefault (liftAtDepthX n d x)

	PData _ bs 
         -> let d' = d + countBAnons bs
	    in  AAlt p (liftAtDepthX n d' x)
        

liftAtDepthXLets
        :: forall a n. Ord n
        => Int             -- ^ Number of levels to lift.
        -> Int             -- ^ Current binding depth.
        -> Lets a n        -- ^ Lift exp indices in this thing.
        -> (Lets a n, Int) -- ^ Lifted, and how much to increase depth by

liftAtDepthXLets n d lts
 = case lts of
        LLet m b x
         -> let inc = countBAnons [b]
                x'  = liftAtDepthX n (d+inc) x
            in  (LLet m b x', inc)

        LRec bs
         -> let inc = countBAnons (map fst bs)
                bs' = map (\(b,e) -> (b, liftAtDepthX n (d+inc) e)) bs
            in  (LRec bs', inc)

        LLetRegion _b bs -> (lts, countBAnons bs)
        LWithRegion _    -> (lts, 0)


countBAnons = length . filter isAnon
 where	isAnon (BAnon _) = True
	isAnon _	 = False


