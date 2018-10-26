
module DDC.Core.Transform.MapT
        (mapT)
where
import DDC.Core.Exp.Annot.Exp

type  MAPT m c n
        = (Type n -> m (Type n)) -> c n -> m (c n)


class Monad m => MapT m (c :: * -> *) where
 -- | Apply a function to all possibly open types in a thing.
 --   Not the types of primitives because they're guaranteed to
 --   be closed.
 mapT :: forall n. MAPT m c n


instance Monad m => MapT m (Exp a) where
 mapT :: forall n. MAPT  m (Exp a) n
 mapT f xx
  = let down :: forall (c :: * -> *). (Monad m, MapT m c) => c n -> m (c n)
        down = mapT f
    in case xx of
        XVar   a u       -> pure (XVar  a u)
        XAbs   a b x     -> XAbs   a <$> down b   <*> down x
        XApp   a x1 x2   -> XApp   a <$> down x1  <*> down x2
        XLet   a lts x   -> XLet   a <$> down lts <*> down x
        XAtom  a t       -> pure (XAtom a t)
        XCase  a x alts  -> XCase  a <$> down x   <*> mapM down alts
        XCast  a cc x    -> XCast  a <$> down cc  <*> down x
        XAsync a b e1 e2 -> XAsync a <$> down b   <*> down e1 <*> down e2


instance Monad m => MapT m Param where
 mapT :: forall n. MAPT m Param n
 mapT f pp
  = case pp of
        MType b         -> MType     <$> mapT f b
        MTerm b         -> MTerm     <$> mapT f b
        MImplicit b     -> MImplicit <$> mapT f b


instance Monad m => MapT m (Arg a) where
 mapT :: forall n. MAPT m (Arg a) n
 mapT f aa
  = case aa of
        RType t         -> RType     <$> f t
        RTerm x         -> RTerm     <$> mapT f x
        RImplicit x     -> RImplicit <$> mapT f x
        RWitness  w     -> RWitness  <$> mapT f w


instance Monad m => MapT m (Lets a) where
 mapT :: forall n. MAPT m (Lets a) n
 mapT f lts
  = let down :: forall (c :: * -> *). (Monad m, MapT m c) => c n -> m (c n)
        down =  mapT f
    in case lts of

        LLet b x
         -> LLet <$> down b <*> down x

        LRec bxs
         -> do  let (bs, xs)  = unzip bxs
                bs'     <- mapM down bs
                xs'     <- mapM down xs
                return  $ LRec $ zip bs' xs'

        LPrivate bs mT ws
         -> do  bs'     <- mapM down bs
                mT'     <- case mT of
                                Nothing -> return Nothing
                                Just t  -> fmap Just $ f t
                ws'     <- mapM down ws

                return  $ LPrivate bs' mT' ws'


instance Monad m => MapT m (Alt a) where
 mapT :: forall n. MAPT m (Alt a) n
 mapT f alt
  = let down :: forall (c :: * -> *). (Monad m, MapT m c) => c n -> m (c n)
        down =  mapT f
    in case alt of
        AAlt u x        -> AAlt  <$> down u <*> down x


instance Monad m => MapT m Pat where
 mapT :: forall n. MAPT m Pat n
 mapT f pat
  = let down :: forall (c :: * -> *). (Monad m, MapT m c) => c n -> m (c n)
        down =  mapT f
    in case pat of
        PDefault        -> pure PDefault
        PData dc bs     -> PData dc <$> mapM down bs


instance Monad m => MapT m (Witness a) where
 mapT :: forall n. MAPT m (Witness a) n
 mapT f ww
  = let down :: forall (c :: * -> *). (Monad m, MapT m c)  => c n -> m (c n)
        down =  mapT f
    in case ww of
        WVar a u        -> WVar  a <$> down u
        WCon{}          -> pure ww
        WApp  a w1 w2   -> WApp  a <$> down w1 <*> down w2
        WType a t       -> WType a <$> f t


instance Monad m => MapT m (Cast a) where
 mapT :: forall n. MAPT m  (Cast a) n
 mapT f cc
  = let down :: forall (c :: * -> *). (Monad m, MapT m c) => c n -> m (c n)
        down =  mapT f
    in case cc of
        CastWeakenEffect t      -> pure $ CastWeakenEffect  t
        CastPurify w            -> CastPurify  <$> down w
        CastBox                 -> pure CastBox
        CastRun                 -> pure CastRun


instance Monad m => MapT m Bind where
 mapT f b
  = case b of
        BNone t         -> BNone   <$> (f t)
        BAnon t         -> BAnon   <$> (f t)
        BName n t       -> BName n <$> (f t)


instance Monad m => MapT m Bound where
 mapT _ u
  = return u


