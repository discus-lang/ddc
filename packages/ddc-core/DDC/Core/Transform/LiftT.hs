
module DDC.Core.Transform.LiftT
        ( liftT,         liftAtDepthT
        , MapBoundT(..))
where
import DDC.Core.Exp
import DDC.Type.Transform.LiftT


instance Ord n => MapBoundT (Exp a) n where
 mapBoundAtDepthT f d xx
  = let down = mapBoundAtDepthT f d
    in case xx of
        XVar a u                -> XVar a   u
        XCon{}                  -> xx
        XApp a x1 x2            -> XApp a   (down x1) (down x2)
        XLAM a b x              -> XLAM a b (mapBoundAtDepthT f (d + countBAnons [b]) x)
        XLam a b x              -> XLam a   (down b) (down x)
         
        XLet a lets x   
         -> let (lets', levels) = mapBoundAtDepthTLets f d lets 
            in  XLet a lets' (mapBoundAtDepthT f (d + levels) x)

        XCase    a x alts       -> XCase    a (down x)  (map down alts)
        XCast    a cc x         -> XCast    a (down cc) (down x)
        XType    a t            -> XType    a (down t)
        XWitness a w            -> XWitness a (down w)

         
instance Ord n => MapBoundT (Witness a) n where
 mapBoundAtDepthT f d ww
  = let down = mapBoundAtDepthT f d
    in case ww of
        WVar  a u               -> WVar  a (down u)
        WCon  _ _               -> ww
        WApp  a w1 w2           -> WApp  a (down w1) (down w2)
        WJoin a w1 w2           -> WJoin a (down w1) (down w2)
        WType a t               -> WType a (down t)


instance Ord n => MapBoundT (Cast a) n where
 mapBoundAtDepthT f d cc
  = let down = mapBoundAtDepthT f d
    in case cc of
        CastWeakenEffect t      -> CastWeakenEffect  (down t)
        CastWeakenClosure xs    -> CastWeakenClosure (map down xs)
        CastPurify w            -> CastPurify (down w)
        CastForget w            -> CastForget (down w)
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

        LLetRegions bsT bsX
         -> let inc  = countBAnons bsT
                bsX' = map (mapBoundAtDepthT f (d + inc)) bsX
            in  ( LLetRegions bsT bsX'
                , inc)

        LWithRegion _
         -> (lts, 0)


countBAnons = length . filter isAnon
 where  isAnon (BAnon _) = True
        isAnon _         = False
