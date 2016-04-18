
module DDC.Core.Transform.MapT
        (mapT)
where
import DDC.Core.Exp.Annot.Exp
import Control.Monad

type  MAPT c n = (Type n -> Type n) -> c n -> c n

class MapT (c :: * -> *) where
 -- | Apply a function to all possibly open types in a thing.
 --   Not the types of primitives because they're guaranteed to
 --   be closed.
 mapT :: forall n. MAPT c n


instance MapT Bind  where
 mapT f b       
  = case b of
        BNone t         -> BNone (f t)
        BAnon t         -> BAnon (f t)
        BName n t       -> BName n (f t)


instance MapT Bound where
 mapT _ u       = u
  

instance MapT (Exp a) where
 mapT :: forall n. MAPT (Exp a) n
 mapT f xx
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down    = mapT f
    in case xx of  
        XVar  a u       -> XVar  a u
        XCon  a c       -> XCon  a c
        XApp  a x1 x2   -> XApp  a (down x1)  (down x2)
        XLAM  a b x     -> XLAM  a (down b)   (down x)
        XLam  a b x     -> XLam  a (down b)   (down x)
        XLet  a lts x   -> XLet  a (down lts) (down x)
        XCase a x alts  -> XCase a (down x)   (map down alts)
        XCast a cc x    -> XCast a (down cc)  (down x)
        XType a t       -> XType a (f t)
        XWitness a w    -> XWitness a (down w)


instance MapT (Lets a) where
 mapT :: forall n. MAPT (Lets a) n
 mapT f lts
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down    = mapT f
    in case lts of
        LLet b x          -> LLet (down b) (down x)
        LRec bxs          -> LRec [ (down b, down x) | (b, x) <- bxs]
        LPrivate bs mT ws -> LPrivate (map down bs) (liftM f mT) (map down ws)


instance MapT (Alt a) where
 mapT :: forall n. MAPT (Alt a) n
 mapT f alt
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down    = mapT f
    in case alt of
        AAlt u x        -> AAlt (down u) (down x)


instance MapT Pat where
 mapT :: forall n. MAPT Pat n
 mapT f pat
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down    = mapT f
    in case pat of
        PDefault        -> PDefault
        PData dc bs     -> PData dc (map down bs)


instance MapT (Witness a) where
 mapT :: forall n. MAPT (Witness a) n
 mapT f ww
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down    = mapT f
    in case ww of
        WVar a u        -> WVar  a (down u)
        WCon{}          -> ww
        WApp  a w1 w2   -> WApp  a (down w1) (down w2)
        WType a t       -> WType a (f t)


instance MapT (Cast a) where
 mapT :: forall n. MAPT (Cast a) n
 mapT f cc
  = let down :: forall (c :: * -> *). MapT c => c n -> c n
        down x  = mapT f x
    in case cc of
        CastWeakenEffect t      -> CastWeakenEffect  t
        CastPurify w            -> CastPurify  (down w)
        CastBox                 -> CastBox
        CastRun                 -> CastRun

