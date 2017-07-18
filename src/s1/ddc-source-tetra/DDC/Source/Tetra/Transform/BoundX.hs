{-# LANGUAGE TypeFamilies #-}
-- | Lifting level-0 deBruijn indices in source expressions.
--
--   Level-0 indices are used for both value and witness variables.
--
module DDC.Source.Tetra.Transform.BoundX
        ( liftX,        liftAtDepthX
        , MapBoundX     (..)
        , HasAnonBind   (..))
where
import DDC.Source.Tetra.Exp.Generic
import DDC.Source.Tetra.Exp.Bind

---------------------------------------------------------------------------------------------------
class HasAnonBind l => MapBoundX (c :: * -> *) l where
 mapBoundAtDepthX
        :: l                                     -- Proxy for language index, not inspected.
        -> (Int -> GXBoundVar l -> GXBoundVar l) -- Function to apply to current bound occ.
        -> Int                                   -- Current binding depth.
        -> c l                                   -- Map across bounds in this thing.
        -> c l                                   -- Result thing.


-- Lift -------------------------------------------------------------------------------------------
-- | Lift debruijn indices less than or equal to the given depth.
liftAtDepthX
        :: (MapBoundX c l, GXBoundVar l ~ Bound)
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
                UIx i
                 | d' <= i      -> UIx (i + n)
                 | otherwise    -> u
                UHole{}         -> u


-- | Wrapper for `liftAtDepthX` that starts at depth 0.
liftX   :: (MapBoundX c l, GXBoundVar l ~ Bound)
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
        XAnnot a x      -> XAnnot  a (downX l f d x)
        XPrim  p        -> XPrim   p
        XFrag  p        -> XFrag   p
        XVar   u        -> XVar   (f d u)
        XCon   c        -> XCon    c
        XApp   x1 r2    -> XApp x1 (downArg l f d r2)

        XAbs     (MType  b mt) x
         -> XAbs (MType  b mt) (downX l f d x)

        XAbs     m@(MTerm p _mt) x
         -> let d'      = d + countBAnonsP l p
            in  XAbs m (mapBoundAtDepthX l f d' x)

        XAbs     m@(MImplicit p _mt) x
         -> let d'      = d + countBAnonsP l p
            in  XAbs m (mapBoundAtDepthX l f d' x)

        XLet   lets x
         -> let (lets', levels) = mapBoundAtDepthXLets l f d lets
            in  XLet lets' (mapBoundAtDepthX l f (d + levels) x)

        XCase x alts      -> XCase (downX l f d x)  (map (downA l f d) alts)
        XCast cc x        -> XCast (downC l f d cc) (downX l f d x)

        XDefix    a rs    -> XDefix    a (map (downArg l f d) rs)
        XInfixOp  a x     -> XInfixOp  a x
        XInfixVar a x     -> XInfixVar a x
        XMatch    a gs x  -> XMatch    a (map (downMA l f d) gs) (downX l f d x)
        XWhere    a x cls -> XWhere    a (downX l f d x) (map (downCL l f d) cls)

        XAbsPat   a ps p mt x
         -> let d'      = d + countBAnonsP l p
            in  XAbsPat a ps p mt (downX l f d' x)

        XLamCase  a alts
         -> XLamCase a (map (downA l f d) alts)


instance HasAnonBind l => MapBoundX GArg l where
 mapBoundAtDepthX = downArg

downArg l f d arg
 = case arg of
        RType{}         -> arg
        RWitness w      -> RWitness  (downW   l f d w)
        RTerm x         -> RTerm     (downX   l f d x)
        RImplicit arg'  -> RImplicit (downArg l f d arg')


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GClause l where
 mapBoundAtDepthX = downCL

downCL l f d cc
  = case cc of
        SSig{}          -> cc
        SLet a b p gs   -> SLet a b p (map (downGX l f d) gs)


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GAltCase l where
 mapBoundAtDepthX = downA

downA l f d (AAltCase p gxs)
  = case p of
        PDefault
         -> AAltCase PDefault (map (downGX l f d)  gxs)

        PAt b p'
         -> let d'                = d + countBAnonsB l [b]
                AAltCase p'' gxs' = downA l f d' (AAltCase p' gxs)
            in  AAltCase (PAt b p'') gxs'

        PVar b
         -> let d' = d + countBAnonsB l [b]
            in  AAltCase p (map (downGX l f d') gxs)

        PData _ ps
         -> let d' = d + (sum $ map (countBAnonsP l) ps)
            in  AAltCase p (map (downGX l f d') gxs)


---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GAltMatch l where
 mapBoundAtDepthX = downMA

downMA l f d (AAltMatch gx)
  = AAltMatch (downGX l f d gx)


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
        CastBox         -> CastBox
        CastRun         -> CastRun

---------------------------------------------------------------------------------------------------
instance HasAnonBind l => MapBoundX GWitness l where
 mapBoundAtDepthX = downW

downW l f d ww
 = case ww of
        WAnnot a w      -> WAnnot a (downW l f d w)
        WVar   u        -> WVar   (f d u)
        WCon   c        -> WCon   c
        WApp   w1 w2    -> WApp   (downW l f d w1) (downW l f d w2)
        WType  t        -> WType  t


---------------------------------------------------------------------------------------------------
mapBoundAtDepthXLets
        :: forall l
        .  HasAnonBind l
        => l
        -> (Int -> GXBoundVar l -> GXBoundVar l)
                           -- ^ Function given number of levels to lift.
        -> Int             -- ^ Current binding depth.
        -> GLets l         -- ^ Lift exp indices in this thing.
        -> (GLets l, Int)  -- ^ Lifted, and how much to increase depth by

mapBoundAtDepthXLets l f d lts
 = case lts of
        LLet b x
         -> let inc = countBAnonsBM l [b]
                x'  = mapBoundAtDepthX l f d x
            in  (LLet b x', inc)

        LRec bs
         -> let inc = countBAnonsBM l (map fst bs)
                bs' = map (\(b,e) -> (b, mapBoundAtDepthX l f (d + inc) e)) bs
            in  (LRec bs', inc)

        LPrivate _b _ bts
         -> (lts, countBAnonsB l $ map fst bts)

        -- ISSUE #430: In BoundX transform, management of debruijn depth is wrong.
        LGroup bRec cs
         -> let inc = sum (map (countBAnonsC l) cs)
                cs' = map (mapBoundAtDepthX l f (d + inc)) cs
            in  (LGroup bRec cs', inc)


---------------------------------------------------------------------------------------------------
countBAnonsB  :: HasAnonBind l => l -> [GXBindVar l] -> Int
countBAnonsB l = length . filter (isAnon l)

countBAnonsBM  :: HasAnonBind l => l -> [GXBindVarMT l] -> Int
countBAnonsBM l bmts
        = length
        $ filter (isAnon l)
        $ [b | XBindVarMT b _ <- bmts]


countBAnonsC :: HasAnonBind l => l -> GClause l -> Int
countBAnonsC l c
 = case c of
        SSig _ b  _   -> countBAnonsB  l [b]
        SLet _ bm _ _ -> countBAnonsBM l [bm]


countBAnonsG :: HasAnonBind l => l -> GGuard l -> Int
countBAnonsG l g
 = case g of
        GPat p _    -> countBAnonsP l p
        GPred _     -> 0
        GDefault    -> 0

countBAnonsP :: HasAnonBind l => l -> GPat l -> Int
countBAnonsP l p
 = case p of
        PData _  ps -> sum $ map (countBAnonsP l) ps
        PAt   b p'  -> countBAnonsB l [b] + countBAnonsP l p'
        PVar  b     -> countBAnonsB l [b]
        PDefault    -> 0


