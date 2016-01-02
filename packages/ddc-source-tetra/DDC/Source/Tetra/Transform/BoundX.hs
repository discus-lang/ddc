{-# LANGUAGE TypeFamilies #-}
-- | Lifting and lowering level-0 deBruijn indices in source expressions.
--
--   Level-0 indices are used for both value and witness variables.
--
module DDC.Source.Tetra.Transform.BoundX
        ( liftX,        liftAtDepthX
--        , lowerX,       lowerAtDepthX
        , MapBoundX     (..)
        , HasAnonBind   (..))
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Type.Exp

---------------------------------------------------------------------------------------------------
class HasAnonBind l => MapBoundX (c :: * -> *) l where
 mapBoundAtDepthX
        :: l                                    -- Proxy for language index, not inspected.
        -> (Int -> GBound l -> GBound l)        -- Function to apply to current bound occ.
        -> Int                                  -- Current binding depth.
        -> c l                                  -- Map across bounds in this thing.
        -> c l                                  -- Result thing.


-- Lift -------------------------------------------------------------------------------------------
-- | Lift debruijn indices less than or equal to the given depth.
liftAtDepthX   
        :: (MapBoundX c l, GBound l ~ Bound n)
        => l
        -> Int          -- ^ Number of levels to lift.
        -> Int          -- ^ Current binding depth.
        -> c l          -- ^ Lift expression indices in this thing.
        -> c l

liftAtDepthX l n d
 = mapBoundAtDepthX l liftU d
 where  
        liftU d' u
         = case u of
                UName{}         -> u
                UPrim{}         -> u
                UIx i
                 | d' <= i      -> UIx (i + n)
                 | otherwise    -> u


-- | Wrapper for `liftAtDepthX` that starts at depth 0.       
liftX   :: (MapBoundX c l, GBound l ~ Bound n)
        => Int -> c l -> c l

liftX n xx  
 = let lProxy :: l
       lProxy = error "ddc-source-tetra.lProxy"
   in  liftAtDepthX lProxy n 0 xx


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GExp l where
 mapBoundAtDepthX = downX 

downX l f d xx
  = case xx of
        XVar  a u       -> XVar a (f d u)
        XCon{}          -> xx
        XPrim{}         -> xx
        XApp  a x1 x2   -> XApp a   (downX l f d x1) (downX l f d x2)
        XLAM  a b x     -> XLAM a b (downX l f d x)

        XLam  a b x     
         -> let d'      = d + countBAnons l [b]
            in  XLam a b (mapBoundAtDepthX l f d' x)

        XLet  a lets x   
         -> let (lets', levels) = mapBoundAtDepthXLets l f d lets 
            in  XLet a lets' (mapBoundAtDepthX l f (d + levels) x)

        XCase a x alts  -> XCase a  (downX l f d x)  (map (downA l f d) alts)
        XCast a cc x    -> XCast a  (downC l f d cc) (downX l f d x)
        XType{}         -> xx
        XWitness a w    -> XWitness a (downW l f d w)

        XDefix   a xs   -> XDefix a (map (downX l f d) xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GClause l where
 mapBoundAtDepthX = downCL

downCL l f d cc
  = case cc of
        SSig{}          -> cc
        SLet a b p gs   -> SLet a b p (map (downGX l f d) gs)


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GAlt l where
 mapBoundAtDepthX = downA

downA l f d (AAlt p gxs)
  = case p of
        PDefault 
         -> AAlt PDefault (map (downGX l f d)  gxs)

        PData _ bs 
         -> let d' = d + countBAnons l bs
            in  AAlt p    (map (downGX l f d') gxs)


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GGuardedExp l where
 mapBoundAtDepthX = downGX

downGX l f d gx
  = case gx of
        GGuard g gxs    
         -> let d' = d + countBAnonsG l g
            in  GGuard (downG l f d g) (downGX l f d' gxs)

        GExp x  
         -> GExp   (downX l f d x)


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GGuard l where
 mapBoundAtDepthX = downG

downG l f d g
 = case g of
        GPat p x        -> GPat p (downX l f d x)
        GPred x         -> GPred  (downX l f d x)
        GDefault        -> GDefault


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GCast l where
 mapBoundAtDepthX = downC

downC _l _f _d cc
  = case cc of
        CastWeakenEffect{} -> cc
        CastPurify w    -> CastPurify w
        CastBox         -> CastBox
        CastRun         -> CastRun

---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GWitness l where
 mapBoundAtDepthX = downW

downW l f d ww
 = case ww of
        WVar a u        -> WVar a (f d u)
        WCon{}          -> ww
        WApp a w1 w2    -> WApp a (downW l f d w1) (downW l f d w2)
        WType{}         -> ww


---------------------------------------------------------------------------------------------------
mapBoundAtDepthXLets
        :: forall l
        .  HasAnonBind l
        => l
        -> (Int -> GBound l -> GBound l)  
                           -- ^ Function given number of levels to lift.
        -> Int             -- ^ Current binding depth.
        -> GLets l         -- ^ Lift exp indices in this thing.
        -> (GLets l, Int)  -- ^ Lifted, and how much to increase depth by

mapBoundAtDepthXLets l f d lts
 = case lts of
        LLet b x
         -> let inc = countBAnons l [b]
                x'  = mapBoundAtDepthX l f d x
            in  (LLet b x', inc)

        LRec bs
         -> let inc = countBAnons l (map fst bs)
                bs' = map (\(b,e) -> (b, mapBoundAtDepthX l f (d + inc) e)) bs
            in  (LRec bs', inc)

        LPrivate _b _ bs 
         -> (lts, countBAnons l bs)

        LGroup cs
         -> let inc = sum (map (countBAnonsC l) cs)
                cs' = map (mapBoundAtDepthX l f (d + inc)) cs
            in  (LGroup cs', inc)


---------------------------------------------------------------------------------------------------
countBAnons  :: HasAnonBind l => l -> [GBind l] -> Int
countBAnons l = length . filter (isAnon l)


countBAnonsC :: HasAnonBind l => l -> GClause l -> Int
countBAnonsC l c
 = case c of
        SSig _ b _   -> countBAnons l [b]
        SLet _ b _ _ -> countBAnons l [b]


countBAnonsG :: HasAnonBind l => l -> GGuard l -> Int
countBAnonsG l g
 = case g of
        GPat p _    -> countBAnonsP l p 
        GPred _     -> 0
        GDefault    -> 0

countBAnonsP :: HasAnonBind l => l -> GPat l -> Int
countBAnonsP l p
 = case p of
        PData _  bs -> countBAnons l bs
        PDefault    -> 0

