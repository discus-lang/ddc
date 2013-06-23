
-- | Lifting and lowering level-0 deBruijn indices in core things.
-- 
--   Level-0 indices are used for both value and witness variables.
module DDC.Core.Transform.LiftX
        ( liftX,        liftAtDepthX
        , lowerX,       lowerAtDepthX
        , MapBoundX(..))
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
 = {-# SCC liftAtDepthX #-} 
   mapBoundAtDepthX liftU d
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
 = {-# SCC lowerAtDepthX #-}
   mapBoundAtDepthX liftU d
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
 -- | Apply a function to all bound variables in the program.
 --   The function is passed the current binding depth.
 --   This is used to defined both `liftX` and `lowerX`.
 mapBoundAtDepthX
        :: (Int -> Bound n -> Bound n)  -- ^ Function to apply to the bound occ.
                                        --   It is passed the current binding depth.
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
        XWitness w	-> XWitness (down w)


instance MapBoundX (LetMode a) n where
 mapBoundAtDepthX f d m
  = case m of
        LetStrict        -> m
        LetLazy Nothing  -> m
        LetLazy (Just w) -> LetLazy (Just $ mapBoundAtDepthX f d w)

         
instance MapBoundX (Witness a) n where
 mapBoundAtDepthX f d ww
  = let down = mapBoundAtDepthX f d
    in case ww of
        WVar  a u       -> WVar  a (down u)
	WCon  _ _       -> ww
	WApp  a w1 w2   -> WApp  a (down w1) (down w2)
	WJoin a w1 w2   -> WJoin a (down w1) (down w2)
	WType _ _       -> ww


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
                m'  = mapBoundAtDepthX f d m

		-- non-recursive binding: do not increase x's depth
                x'  = mapBoundAtDepthX f d x
            in  (LLet m' b x', inc)

        LRec bs
         -> let inc = countBAnons (map fst bs)
                bs' = map (\(b,e) -> (b, mapBoundAtDepthX f (d+inc) e)) bs
            in  (LRec bs', inc)

        LLetRegions _b bs -> (lts, countBAnons bs)
        LWithRegion _     -> (lts, 0)


countBAnons = length . filter isAnon
 where	isAnon (BAnon _) = True
	isAnon _	 = False


