
-- | Boilerplate to apply a function to every name in a `Type` related thing.
module DDC.Type.Operators.LiftNames
        ( liftNamesT
        , liftNamesB
        , liftNamesU
        , liftNamesC
        , liftNamesTC)
where
import DDC.Type.Exp

-- | Lift a function to the names of a `Type`.
liftNamesT :: (n1 -> n2) -> Type n1 -> Type n2
liftNamesT f tt
 = case tt of
        TVar    u       -> TVar    (liftNamesU f u)
        TCon    c       -> TCon    (liftNamesC f c)
        TForall b t     -> TForall (liftNamesB f b) (liftNamesT f t)
        TApp    t1 t2   -> TApp    (liftNamesT f t1) (liftNamesT f t2)
        TSum    ts      -> TSum    (liftNamesTS f ts)
        TBot    k       -> TBot    (liftNamesT f k)


-- | Lift a function to the types of a `TypeSum`.
liftNamesTS :: (n1 -> n2) -> TypeSum n1 -> TypeSum n2
liftNamesTS = error "foo!"


-- | Lift a function to the names of a `Bind`.
liftNamesB :: (n1 -> n2) -> Bind n1 -> Bind n2
liftNamesB f bb
 = case bb of
        BName n k       -> BName (f n) (liftNamesT f k)
        BAnon   k       -> BAnon       (liftNamesT f k)
        

-- | Lift a function to the names of a `Bound`.
liftNamesU :: (n1 -> n2) -> Bound n1 -> Bound n2
liftNamesU f uu
 = case uu of
        UName n k       -> UName (f n) (liftNamesT f k)
        UIx   i k       -> UIx   i     (liftNamesT f k)


-- | Lift a function to the names of a `TCon`.
liftNamesC :: (n1 -> n2) -> TCon n1 -> TCon n2
liftNamesC f c
 = case c of
        TConSort sc     -> TConSort sc
        TConKindFun     -> TConKindFun
        TConKind kc     -> TConKind kc
        TConType tc     -> TConType (liftNamesTC f tc)


-- | Lift a function to the names of a `TyCon`
liftNamesTC :: (n1 -> n2) -> TyCon n1 -> TyCon n2
liftNamesTC f tc
 = case tc of
        TyConUser n k   -> TyConUser (f n) (liftNamesT f k)
        TyConBuiltin tb -> TyConBuiltin tb


