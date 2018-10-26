
-- | Lifting and lowering level-1 deBruijn indices in code things.
--
--   Level-1 indices are used for type variables.
--
module DDC.Core.Transform.BoundT
        ( liftT,         liftAtDepthT
        , MapBoundT(..))
where
import DDC.Core.Exp.Annot.Exp
import DDC.Type.Transform.BoundT


instance Ord n => MapBoundT (Exp a) n where
 mapBoundAtDepthT f d xx
  = let down = mapBoundAtDepthT f d
    in case xx of
        XVar a u                -> XVar a   u

        XAbs a (MType b) x      -> XAbs a (MType b) (mapBoundAtDepthT f (d + countBAnons [b]) x)
        XAbs a (MTerm b) x      -> XAbs a (MTerm     (down b)) (down x)
        XAbs a (MImplicit b) x  -> XAbs a (MImplicit (down b)) (down x)

        XApp a x1 x2            -> XApp a (down x1) (down x2)

        XLet a lets x
         -> let (lets', levels) = mapBoundAtDepthTLets f d lets
            in  XLet a lets' (mapBoundAtDepthT f (d + levels) x)

        XAtom {}                -> xx
        XCase a x alts          -> XCase    a (down x)  (map down alts)
        XCast a cc x            -> XCast    a (down cc) (down x)
        XAsync a b e1 e2        -> XAsync   a (down b)  (down e1) (down e2)


instance Ord n => MapBoundT (Arg a) n where
 mapBoundAtDepthT f d aa
  = let down = mapBoundAtDepthT f d
    in case aa of
        RType t                 -> RType     (down t)
        RTerm x                 -> RTerm     (down x)
        RImplicit x             -> RImplicit (down x)
        RWitness  x             -> RWitness  (down x)


instance Ord n => MapBoundT (Witness a) n where
 mapBoundAtDepthT f d ww
  = let down = mapBoundAtDepthT f d
    in case ww of
        WVar  a u               -> WVar  a (down u)
        WCon  _ _               -> ww
        WApp  a w1 w2           -> WApp  a (down w1) (down w2)
        WType a t               -> WType a (down t)


instance Ord n => MapBoundT (Cast a) n where
 mapBoundAtDepthT f d cc
  = let down = mapBoundAtDepthT f d
    in case cc of
        CastWeakenEffect t      -> CastWeakenEffect  (down t)
        CastPurify w            -> CastPurify (down w)
        CastBox                 -> CastBox
        CastRun                 -> CastRun


instance Ord n => MapBoundT (Alt a) n where
 mapBoundAtDepthT f d (AAlt p x)
  = let down = mapBoundAtDepthT f d
    in case p of
        PDefault                -> AAlt PDefault (down x)
        PData dc bs             -> AAlt (PData dc (map down bs)) (down x)


mapBoundAtDepthTLets
        :: Ord n
        => (Int -> Bound n -> Bound n)  -- ^ Number of levels to lift.
        -> Int                          -- ^ Current binding depth.
        -> Lets a n                     -- ^ Lift exp indices in this thing.
        -> (Lets a n, Int)              -- ^ Lifted, and how much to increase depth by

mapBoundAtDepthTLets f d lts
 = let down = mapBoundAtDepthT f d
   in case lts of
        LLet b x
         ->     ( LLet (down b) (down x)
                , 0)

        LRec bs
         -> let bs' = [ (b, mapBoundAtDepthT f d x) | (b, x) <- bs ]
            in  (LRec bs', 0)

        LPrivate bsT mT bsX
         -> let inc  = countBAnons bsT
                bsX' = map (mapBoundAtDepthT f (d + inc)) bsX
            in  ( LPrivate bsT mT bsX'
                , inc)


countBAnons = length . filter isAnon
 where  isAnon (BAnon _) = True
        isAnon _         = False
