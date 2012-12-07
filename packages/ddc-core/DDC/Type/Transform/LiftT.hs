
-- | Lifting of deBruijn indices in a type.
module DDC.Type.Transform.LiftT
        ( liftT,        liftAtDepthT
        , lowerT,       lowerAtDepthT
        , MapBoundT(..))
where
import DDC.Type.Exp
import DDC.Type.Compounds
import qualified DDC.Type.Sum   as Sum


-- Lift -----------------------------------------------------------------------
-- | Lift debruijn indices less than or equal to the given depth.
liftAtDepthT
        :: MapBoundT c n
        => Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift expression indices in this thing.
        -> c n

liftAtDepthT n d
 = mapBoundAtDepthT liftU d
 where  
        liftU d' u
         = case u of
                UName{}         -> u
                UPrim{}         -> u
                UIx i
                 | d' <= i      -> UIx (i + n)
                 | otherwise    -> u


-- | Wrapper for `liftAtDepthX` that starts at depth 0.       
liftT   :: MapBoundT c n => Int -> c n -> c n
liftT n xx  = liftAtDepthT n 0 xx


-- Lower ----------------------------------------------------------------------
-- | Lower debruijn indices less than or equal to the given depth.
lowerAtDepthT
        :: MapBoundT c n
        => Int          -- ^ Number of levels to lower.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lower expression indices in this thing.
        -> c n

lowerAtDepthT n d
 = mapBoundAtDepthT lowerU d
 where  
        lowerU d' u
         = case u of
                UName{}         -> u
                UPrim{}         -> u
                UIx i
                 | d' <= i      -> UIx (i - n)
                 | otherwise    -> u


-- | Wrapper for `lowerAtDepthX` that starts at depth 0.       
lowerT   :: MapBoundT c n => Int -> c n -> c n
lowerT n xx  = lowerAtDepthT n 0 xx


-- MapBoundT ------------------------------------------------------------------
class MapBoundT (c :: * -> *) n where
 -- | Apply a function to all bound variables in the program.
 --   The function is passed the current binding depth.
 --   This is used to defined both `liftT` and `lowerT`.
 mapBoundAtDepthT
        :: (Int -> Bound n -> Bound n)  
                        -- ^ Function to apply to the bound occ.
                        --   It is passed the current binding depth.
        -> Int          -- ^ Current binding depth.
        -> c n          -- ^ Lift expression indices in this thing.
        -> c n


instance Ord n => MapBoundT Bind n where
 mapBoundAtDepthT f d bb
  = replaceTypeOfBind (mapBoundAtDepthT f d $ typeOfBind bb) bb


instance MapBoundT Bound n where
 mapBoundAtDepthT f d u
        = f d u


instance Ord n => MapBoundT Type n where
 mapBoundAtDepthT f d tt
  = let down = mapBoundAtDepthT f d
    in case tt of
        TVar u          -> TVar    (f d u)
        TCon{}          -> tt
        TForall b t     -> TForall b (mapBoundAtDepthT f (d + countBAnons [b]) t)
        TApp t1 t2      -> TApp    (down t1) (down t2)
        TSum ss         -> TSum    (down ss)


instance Ord n => MapBoundT TypeSum n where
 mapBoundAtDepthT f d ss
  = Sum.fromList (Sum.kindOfSum ss)
        $ map (mapBoundAtDepthT f d)
        $ Sum.toList ss

countBAnons = length . filter isAnon
 where	isAnon (BAnon _) = True
	isAnon _	 = False

