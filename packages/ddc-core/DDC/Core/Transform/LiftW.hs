
-- | Lift witness variable indices.
module DDC.Core.Transform.LiftW
        (LiftW(..))
where
import DDC.Core.Exp


class LiftW (c :: * -> *) where
 -- | Lift indices that are at least a the given depth by some number
 --   of levels
 liftAtDepthW
        :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift witness variable indices in this thing.
        -> c n
 
 -- | Wrapper for `liftAtDepthX` that starts at depth 0.       
 liftW  :: forall n. Ord n
        => Int          -- ^ Number of levels to lift.
        -> c n          -- ^ Lift witness variable indices in this thing.
        -> c n
        
 liftW n xx  = liftAtDepthW n 0 xx
  

instance LiftW Bound where
 liftAtDepthW n d uu
  = case uu of
        UName{}         -> uu
        UPrim{}         -> uu
        UIx i t 
         | d <= i       -> UIx (i + n) t
         | otherwise    -> uu


instance LiftW (Exp a) where
 liftAtDepthW n d xx
  = let down = liftAtDepthW n d
    in case xx of
        XVar{}          -> xx
        XCon{}          -> xx
        XApp a x1 x2    -> XApp a (down x1) (down x2)
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLam a b (liftAtDepthW n (d + 1) x)
         
        XLet a lets x   
         -> let (lets', levels) = liftAtDepthXLets n d lets 
            in  XLet a lets' (liftAtDepthW n (d + levels) x)

        XCase a x alts  -> XCase a (down x) (map down alts)
        XCast a cc x    -> XCast a cc (down x)
        XType{}         -> xx
        XWitness w      -> XWitness (down w)
         

instance LiftW LetMode where
 liftAtDepthW n d m
  = case m of
        LetStrict        -> m
        LetLazy Nothing  -> m
        LetLazy (Just w) -> LetLazy (Just $ liftAtDepthW n d w)


instance LiftW (Alt a) where
 liftAtDepthW n d (AAlt p x)
  = case p of
	PDefault 
         -> AAlt PDefault (liftAtDepthW n d x)

	PData _ bs 
         -> let d' = d + countBAnons bs
	    in  AAlt p (liftAtDepthW n d' x)


instance LiftW Witness where
 liftAtDepthW n d ww
  = let down = liftAtDepthW n d
    in case ww of
        WVar  u         -> WVar (down u)
        WCon{}          -> ww
        WApp  w1 w2     -> WApp  (down w1) (down w2)
        WJoin w1 w2     -> WJoin (down w1) (down w2)
        WType{}         -> ww
        

liftAtDepthXLets
        :: forall a n. Ord n
        => Int             -- ^ Number of levels to lift.
        -> Int             -- ^ Current binding depth.
        -> Lets a n        -- ^ Lift exp indices in this thing.
        -> (Lets a n, Int) -- ^ Lifted, and how much to increase depth by

liftAtDepthXLets n d lts
 = case lts of
        LLet m b x
         -> let m'  = liftAtDepthW n d m
                inc = countBAnons [b]
                x'  = liftAtDepthW n (d+inc) x
            in  (LLet m' b x', inc)

        LRec bs
         -> let inc = countBAnons (map fst bs)
                bs' = map (\(b,e) -> (b, liftAtDepthW n (d+inc) e)) bs
            in  (LRec bs', inc)

        LLetRegion _b bs -> (lts, countBAnons bs)
        LWithRegion _    -> (lts, 0)


countBAnons = length . filter isAnon
 where	isAnon (BAnon _) = True
	isAnon _	 = False


