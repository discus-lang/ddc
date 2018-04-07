{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Source.Discus.Exp.Type.Compounds
        ( -- * Destructors
          takeTCon
        , takeTVar
        , takeTAbs
        , takeTApp

          -- * Type Applications
        , makeTApps,    takeTApps

          -- * Function Types
        , makeTFun,     makeTFuns,      makeTFuns',     (~>)
        , takeTFun,     takeTFuns,      takeTFuns'

          -- * Forall Types
        , makeTForall,  makeTForalls
        , takeTForall

          -- * Exists Types
        , makeTExists,  takeTExists

          -- * Union types
        , takeTUnion
        , makeTUnions,  takeTUnions
        , splitTUnionsOfKind)
where
import DDC.Source.Discus.Exp.Type.Exp
import DDC.Source.Discus.Exp.Type.Binding


-- Destructors ----------------------------------------------------------------
-- | Take a type constructor, looking through annotations.
takeTCon :: GType l -> Maybe (GTyCon l)
takeTCon tt
 = case tt of
        TAnnot _ t      -> takeTCon t
        TCon   tc       -> Just tc
        _               -> Nothing


-- | Take a type variable, looking through annotations.
takeTVar :: GType l -> Maybe (GTBoundVar l)
takeTVar tt
 = case tt of
        TAnnot _ t      -> takeTVar t
        TVar u          -> Just u
        _               -> Nothing


-- | Take a type abstraction, looking through annotations.
takeTAbs :: GType l -> Maybe (GTBindVar l, GType l, GType l)
takeTAbs tt
 = case tt of
        TAnnot _ t      -> takeTAbs t
        TAbs b k t      -> Just (b, k, t)
        _               -> Nothing


-- | Take a type application, looking through annotations.
takeTApp :: GType l -> Maybe (GType l, GType l)
takeTApp tt
 = case tt of
        TAnnot _ t      -> takeTApp t
        TApp t1 t2      -> Just (t1, t2)
        _               -> Nothing


-- Type Applications ----------------------------------------------------------
-- | Construct a sequence of type applications.
makeTApps :: GType l -> [GType l] -> GType l
makeTApps t1 ts     = foldl TApp t1 ts


-- | Flatten a sequence of type applications into the function part and
--   arguments, if any.
takeTApps :: GType l -> [GType l]
takeTApps tt
 = case takeTApp tt of
        Just (t1, t2) -> takeTApps t1 ++ [t2]
        _             -> [tt]


-- Function Types -------------------------------------------------------------
-- | Construct a function type with the given parameter and result type.
makeTFun :: GType l -> GType l -> GType l
makeTFun t1 t2           = TFunExplicit t1 t2
infixr `makeTFun`

(~>) = makeTFun
infixr ~>

-- | Like `makeFun` but taking a list of parameter types.
makeTFuns :: [GType l] -> GType l -> GType l
makeTFuns []     t1     = t1
makeTFuns (t:ts) t1     = t `makeTFun` makeTFuns ts t1


-- | Like `makeTFuns` but taking the parameter and return types as a list.
makeTFuns' :: [GType l] -> Maybe (GType l)
makeTFuns' []           = Nothing
makeTFuns' [_]          = Nothing
makeTFuns' ts
 = let (tR : tAs)       = reverse ts
   in  Just $ makeTFuns (reverse tAs) tR


-- | Destruct a function type into its parameter and result types,
--   returning `Nothing` if this isn't a function type.
takeTFun :: GType l -> Maybe (GType l, GType l)
takeTFun tt
        | Just (t1f, t2)                   <- takeTApp tt
        , Just (TCon TyConFunExplicit, t1) <- takeTApp t1f
        = Just (t1, t2)

        | otherwise
        = Nothing


-- | Destruct a function type into into all its parameters and result type,
--   returning an empty parameter list if this isn't a function type.
takeTFuns :: GType l -> ([GType l], GType l)
takeTFuns tt
 = case takeTFun tt of
        Just (t1, t2)
          |  (ts, t2') <- takeTFuns t2
          -> (t1 : ts, t2')

        _ -> ([], tt)


-- | Like `takeFuns`, but yield the parameter and return types in the same list.
takeTFuns' :: GType l -> [GType l]
takeTFuns' tt
 = let  (ts, t1) = takeTFuns tt
   in   ts ++ [t1]


-- Forall types ---------------------------------------------------------------
-- | Construct a forall quantified type using an anonymous binder.
makeTForall :: Anon l  => l -> GType l -> (GType l -> GType l) -> GType l
makeTForall l k makeBody
        =  withBinding l $ \b u
        -> TApp (TCon (TyConForall k)) (TAbs b k (makeBody (TVar u)))


-- | Construct a forall quantified type using some anonymous binders.
makeTForalls :: Anon l => l -> [GType l] -> ([GType l] -> GType l) -> GType l
makeTForalls l ks makeBody
        = withBindings l (length ks) $ \bs us
        -> foldr (\(k, b) t -> TApp (TCon (TyConForall k)) (TAbs b k t))
                        (makeBody $ reverse $ map TVar us)
                        (zip ks bs)


-- | Destruct a forall quantified type, if this is one.
--
--   The kind we return comes from the abstraction rather than the
--   Forall constructor.
takeTForall :: GType l -> Maybe (GType l, GTBindVar l, GType l)
takeTForall tt
        | Just (t1, t2)         <- takeTApp tt
        , Just (TyConForall _)  <- takeTCon t1
        , Just (b, k, t)        <- takeTAbs t2
        = Just (k, b, t)

        | otherwise
        = Nothing


-- Exists types ---------------------------------------------------------------
-- | Construct an exists quantified type using an anonymous binder.
makeTExists :: Anon l => l -> GType l -> (GType l -> GType l) -> GType l
makeTExists l k makeBody
        =  withBinding l $ \b u
        -> TApp (TCon (TyConExists k)) (TAbs b k (makeBody (TVar u)))


-- | Destruct an exists quantified type, if this is one.
--
--   The kind we return comes from the abstraction rather than the
--   Exists constructor.
takeTExists :: GType l -> Maybe (GType l, GTBindVar l, GType l)
takeTExists tt
        | Just (t1, t2)         <- takeTApp tt
        , Just (TyConExists _)  <- takeTCon t1
        , Just (b, k, t)        <- takeTAbs t2
        = Just (k, b, t)

        | otherwise
        = Nothing


-- Bot Types ------------------------------------------------------------------
-- | Take a bottom type, looking through annotations.
takeTBot :: GType l -> Maybe (GType l)
takeTBot tt
 = case tt of
        TAnnot _ t              -> takeTBot t
        TCon (TyConBot k)       -> Just k
        _                       -> Nothing


-- Union types ------------------------------------------------------------------
-- | Take the kind, left and right types from a union type.
takeTUnion :: GType l -> Maybe (GType l, GType l, GType l)
takeTUnion tt
        | Just (ts1, t2)        <- takeTApp tt
        , Just (ts,  t1)        <- takeTApp ts1
        , Just (TyConUnion k)   <- takeTCon ts
        = Just (k, t1, t2)

        | otherwise
        = Nothing


-- | Make a union type from a kind and list of component types.
makeTUnions :: GType l -> [GType l] -> GType l
makeTUnions k tss
 = case tss of
        []              -> TBot k
        [t1]            -> t1
        (t1 : ts)       -> foldr (TUnion k) t1 ts


-- | Split a union type into its components.
--    If this is not a union, or is an ill kinded union then Nothing.
takeTUnions :: Eq (GType l) => GType l -> Maybe (GType l, [GType l])
takeTUnions tt
        | Just k                <- takeTBot tt
        = Just (k, [])

        | Just (k, t1, t2)      <- takeTUnion tt
        , Just ts1              <- splitTUnionsOfKind k t1
        , Just ts2              <- splitTUnionsOfKind k t2
        = Just (k, ts1 ++ ts2)

        | otherwise
        = Nothing


-- | Split a union of the given kind into its components.
--    When we split a sum we need to check that the kind attached
--    to the sum type constructor is the one that we were expecting,
--    otherwise we risk splitting ill-kinded sums without noticing it.
splitTUnionsOfKind :: Eq (GType l) => GType l -> GType l -> Maybe [GType l]
splitTUnionsOfKind k t
        | Just k'             <- takeTBot t
        = if k == k'
                then    return []
                else    Nothing

        | Just (k', t1, t2)   <- takeTUnion t
        = if k == k'
                then do t1s     <- splitTUnionsOfKind k t1
                        t2s     <- splitTUnionsOfKind k t2
                        return  $  t1s ++ t2s
                else    Nothing

        | otherwise
        = Just [t]
