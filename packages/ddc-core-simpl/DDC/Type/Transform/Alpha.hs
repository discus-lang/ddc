
module DDC.Type.Transform.Alpha
        (Alpha(..))
where
import DDC.Type.Exp
import DDC.Type.Sum


class Alpha (c :: * -> *) where
 -- | Apply a function to all the names in a thing.
 alpha :: forall n1 n2. Ord n2 => (n1 -> n2) -> c n1 -> c n2
 

instance Alpha Type where
 alpha f tt
  = case tt of
        TVar    u       -> TVar    (alpha f u)
        TCon    c       -> TCon    (alpha f c)
        TForall b t     -> TForall (alpha f b)  (alpha f t)
        TApp    t1 t2   -> TApp    (alpha f t1) (alpha f t2)
        TSum    ts      -> TSum    (alpha f ts)


instance Alpha TypeSum where
 alpha f ts
  = fromList (alpha f $ kindOfSum ts) $ map (alpha f) $ toList ts


instance Alpha Bind where
 alpha f bb
  = case bb of
        BName n t       -> BName (f n) (alpha f t)
        BAnon   t       -> BAnon (alpha f t)
        BNone   t       -> BNone (alpha f t)
        

instance Alpha Bound where
 alpha f uu
  = case uu of
        UIx i           -> UIx i
        UName n         -> UName (f n)
        UPrim n t       -> UPrim (f n) (alpha f t)


instance Alpha TyCon where
 alpha f cc
  = case cc of
        TyConSort sc    -> TyConSort    sc
        TyConKind kc    -> TyConKind    kc
        TyConWitness tc -> TyConWitness tc
        TyConSpec tc    -> TyConSpec    tc
        TyConBound u t  -> TyConBound (alpha f u) (alpha f t)

