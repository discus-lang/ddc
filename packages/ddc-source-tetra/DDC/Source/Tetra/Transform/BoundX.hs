
-- | Lifting and lowering level-0 deBruijn indices in source expressions.
--
--   Level-0 indices are used for both value and witness variables.
--
module DDC.Source.Tetra.Transform.BoundX
        ( liftX,        liftAtDepthX
        , lowerX,       lowerAtDepthX
        , MapBoundX(..))
where
import DDC.Core.Transform.BoundX
import DDC.Source.Tetra.Exp


instance MapBoundX (Exp a) n where
 mapBoundAtDepthX f d xx
  = let down = mapBoundAtDepthX f d
    in case xx of
        XVar  a u       -> XVar a (f d u)
        XCon{}          -> xx
        XApp  a x1 x2   -> XApp a   (down x1) (down x2)
        XLAM  a b x     -> XLAM a b (down x)

        XLam  a b x     
         -> let d'      = d + countBAnons [b]
            in  XLam a b (mapBoundAtDepthX f d' x)

        XLet  a lets x   
         -> let (lets', levels) = mapBoundAtDepthXLets f d lets 
            in  XLet a lets' (mapBoundAtDepthX f (d + levels) x)

        XCase a x alts  -> XCase a  (down x)  (map down alts)
        XCast a cc x    -> XCast a  (down cc) (down x)
        XType{}         -> xx
        XWitness a w    -> XWitness a (down w)

        XDefix   a xs   -> XDefix a (map down xs)
        XInfixOp{}      -> xx
        XInfixVar{}     -> xx


instance MapBoundX (Cast a) n where
 mapBoundAtDepthX _ _ cc
  = case cc of
        CastWeakenEffect{} -> cc
        CastPurify w    -> CastPurify w
        CastBox         -> CastBox
        CastRun         -> CastRun


instance MapBoundX (Alt a) n where
 mapBoundAtDepthX f d (AAlt p gxs)
  = case p of
        PDefault 
         -> AAlt PDefault (map (mapBoundAtDepthX f d)  gxs)

        PData _ bs 
         -> let d' = d + countBAnons bs
            in  AAlt p    (map (mapBoundAtDepthX f d') gxs)


instance MapBoundX (GuardedExp a) n where
 mapBoundAtDepthX f d gx
  = let down = mapBoundAtDepthX f d
    in case gx of
        GGuard g gxs    
         -> let d' = d + countBAnonsG g
            in  GGuard (down g) (mapBoundAtDepthX f d' gxs)

        GExp x  
         -> GExp   (down x)


instance MapBoundX (Guard a) n where
 mapBoundAtDepthX f d g
  = let down = mapBoundAtDepthX f d
    in case g of
        GPat p x        -> GPat p (down x)
        GPred x         -> GPred  (down x)
        GDefault        -> GDefault


mapBoundAtDepthXLets
        :: (Int -> Bound n -> Bound n)  
                           -- ^ Function given number of levels to lift.
        -> Int             -- ^ Current binding depth.
        -> Lets a n        -- ^ Lift exp indices in this thing.
        -> (Lets a n, Int) -- ^ Lifted, and how much to increase depth by

mapBoundAtDepthXLets f d lts
 = case lts of
        LLet b x
         -> let inc = countBAnons [b]
                x'  = mapBoundAtDepthX f d x
            in  (LLet b x', inc)

        LRec bs
         -> let inc = countBAnons (map fst bs)
                bs' = map (\(b,e) -> (b, mapBoundAtDepthX f (d + inc) e)) bs
            in  (LRec bs', inc)

        LPrivate _b _ bs -> (lts, countBAnons bs)


countBAnons = length . filter isAnon
 where  isAnon (BAnon _) = True
        isAnon _         = False

countBAnonsG g
 = case g of
        GPat p _    -> countBAnonsP p 
        GPred _     -> 0
        GDefault    -> 0

countBAnonsP p
 = case p of
        PData _  bs -> countBAnons bs
        PDefault    -> 0

