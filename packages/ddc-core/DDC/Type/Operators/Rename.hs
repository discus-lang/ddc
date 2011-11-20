
-- | Boilerplate to apply a function to every name in a `Type` related thing.
module DDC.Type.Operators.Rename
        (Named(..))
where
import DDC.Type.Exp
import DDC.Type.Sum


class Named (c :: * -> *) where
 rename :: forall n1 n2. Ord n2 => (n1 -> n2) -> c n1 -> c n2

instance Named Type where
 rename f tt
  = case tt of
        TVar    u       -> TVar    (rename f u)
        TCon    c       -> TCon    (rename f c)
        TForall b t     -> TForall (rename f b)  (rename f t)
        TApp    t1 t2   -> TApp    (rename f t1) (rename f t2)
        TSum    ts      -> TSum    (rename f ts)
        TBot    k       -> TBot    (rename f k)


instance Named TypeSum where
 rename f ts
  = fromList (rename f $ kindOfSum ts) $ map (rename f) $ toList ts


instance Named Bind where
 rename f bb
  = case bb of
        BName n k       -> BName (f n) (rename f k)
        BAnon   k       -> BAnon       (rename f k)
        

instance Named Bound where
 rename f uu
  = case uu of
        UName n k       -> UName (f n) (rename f k)
        UIx   i k       -> UIx   i     (rename f k)


instance Named TCon where
 rename f cc
  = case cc of
        TConSort sc     -> TConSort sc
        TConKindFun     -> TConKindFun
        TConKind kc     -> TConKind kc
        TConType tc     -> TConType (rename f tc)


instance Named TyCon where
 rename f tc
  = case tc of
        TyConUser n k   -> TyConUser (f n) (rename f k)
        TyConBuiltin tb -> TyConBuiltin tb

