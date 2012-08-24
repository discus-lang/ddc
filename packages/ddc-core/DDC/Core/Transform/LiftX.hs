
-- | Lift deBruijn indices in expressions.
module DDC.Core.Transform.LiftX
        ( MapBoundX(..)
        , liftX,        liftAtDepthX
        , lowerX,       lowerAtDepthX)
where
import DDC.Core.Exp


-- Lift -----------------------------------------------------------------------
-- | Lift debruijn indices less than or equal to the given depth.
liftAtDepthX   
        :: MapBoundX c n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift expression indices in this thing.
        -> c n

liftAtDepthX n d
 = mapBoundAtDepthX liftU d
 where  
        liftU d' u
         = case u of
                UName{}         -> u
                UPrim{}         -> u
                UIx i
                 | d' <= i      -> UIx (i + n)
                 | otherwise    -> u


-- | Wrapper for `liftAtDepthX` that starts at depth 0.       
liftX   :: MapBoundX c n => Int -> c n -> c n
liftX n xx  = liftAtDepthX n 0 xx


-- Lower ----------------------------------------------------------------------
-- | Lower debruijn indices less than or equal to the given depth.
lowerAtDepthX   
        :: MapBoundX c n
        => Int          -- ^ Number of levels to lower.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lower expression indices in this thing.
        -> c n

lowerAtDepthX n d
 = mapBoundAtDepthX liftU d
 where  
        liftU d' u
         = case u of
                UName{}         -> u
                UPrim{}         -> u
                UIx i
                 | d' <= i      -> UIx (i - n)
                 | otherwise    -> u


-- | Wrapper for `lowerAtDepthX` that starts at depth 0.       
lowerX   :: MapBoundX c n => Int -> c n -> c n
lowerX n xx  = lowerAtDepthX n 0 xx


-- MapBoundX ------------------------------------------------------------------
class MapBoundX (c :: * -> *) n where
 -- | Lift indices that are at least the given depth by some number
 --   of levels.
 mapBoundAtDepthX
        :: (Int -> Bound n -> Bound n)  -- ^ Number of levels to lift.
        -> Int                          -- ^ Current binding depth.
        -> c n                          -- ^ Lift expression indices in this thing.
        -> c n


instance MapBoundX Bound n where
 mapBoundAtDepthX f d u
        = f d u


instance MapBoundX (Exp a) n where
 mapBoundAtDepthX f d xx
  = let down = mapBoundAtDepthX f d
    in case xx of
        XVar a u        -> XVar a (f d u)
        XCon{}          -> xx
        XApp a x1 x2    -> XApp a (down x1) (down x2)
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLam a b (mapBoundAtDepthX f (d + countBAnons [b]) x)
         
        XLet a lets x   
         -> let (lets', levels) = mapBoundAtDepthXLets f d lets 
            in  XLet a lets' (mapBoundAtDepthX f (d + levels) x)

        XCase a x alts  -> XCase a (down x)  (map down alts)
        XCast a cc x    -> XCast a (down cc) (down x)
        XType{}         -> xx
        XWitness{}      -> xx

         
instance MapBoundX (Cast a) n where
 mapBoundAtDepthX f d cc
  = case cc of
        CastWeakenEffect{}
         -> cc

        CastWeakenClosure xs    
         -> CastWeakenClosure (map (mapBoundAtDepthX f d) xs)

        CastPurify w
         -> CastPurify w

        CastForget w
         -> CastForget w


instance MapBoundX (Alt a) n where
 mapBoundAtDepthX f d (AAlt p x)
  = case p of
	PDefault 
         -> AAlt PDefault (mapBoundAtDepthX f d x)

	PData _ bs 
         -> let d' = d + countBAnons bs
	    in  AAlt p (mapBoundAtDepthX f d' x)
        

mapBoundAtDepthXLets
        :: (Int -> Bound n -> Bound n)  -- ^ Number of levels to lift.
        -> Int                          -- ^ Current binding depth.
        -> Lets a n                     -- ^ Lift exp indices in this thing.
        -> (Lets a n, Int)              -- ^ Lifted, and how much to increase depth by

mapBoundAtDepthXLets f d lts
 = case lts of
        LLet m b x
         -> let inc = countBAnons [b]
		-- non-recursive binding: do not increase x's depth
                x'  = mapBoundAtDepthX f d x
            in  (LLet m b x', inc)

        LRec bs
         -> let inc = countBAnons (map fst bs)
                bs' = map (\(b,e) -> (b, mapBoundAtDepthX f (d+inc) e)) bs
            in  (LRec bs', inc)

        LLetRegion _b bs -> (lts, countBAnons bs)
        LWithRegion _    -> (lts, 0)


countBAnons = length . filter isAnon
 where	isAnon (BAnon _) = True
	isAnon _	 = False


