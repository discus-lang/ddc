{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Exp.Generic.Compounds
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
        , makeTExists,  takeTExists)
where
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.Generic.Binding


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
takeTAbs :: GType l -> Maybe (GTBindVar l, GType l)
takeTAbs tt
 = case tt of
        TAnnot _ t      -> takeTAbs t
        TAbs b t        -> Just (b, t)
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
makeTFun t1 t2           = TFun t1 t2
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
        | Just (t1f, t2)               <- takeTApp tt
        , Just (TCon TyConFun, t1)     <- takeTApp t1f
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
 =  withBinding l $ \b u -> TForall k b (makeBody (TVar u))


-- | Construct a forall quantified type using some anonymous binders.
makeTForalls :: Anon l => l -> [GType l] -> ([GType l] -> GType l) -> GType l
makeTForalls l ks makeBody
 = withBindings l (length ks)
 $ \bs us -> foldr (\(k, b) -> TForall k b) 
                   (makeBody $ reverse $ map TVar us)
                   (zip ks bs)


-- | Destruct a forall quantified type, if this is one.
takeTForall :: GType l -> Maybe (GType l, GTBindVar l, GType l)
takeTForall tt
        | Just (t1, t2)         <- takeTApp tt
        , Just (TyConForall k)  <- takeTCon t1
        , Just (b, t)           <- takeTAbs t2
        = Just (k, b, t)

        | otherwise
        = Nothing


-- Exists types ---------------------------------------------------------------
-- | Construct an exists quantified type using an anonymous binder.
makeTExists :: Anon l => l -> GType l -> (GType l -> GType l) -> GType l
makeTExists l k makeBody
 =  withBinding l $ \b u -> TExists k b (makeBody (TVar u))


-- | Destruct an exists quantified type, if this is one.
takeTExists :: GType l -> Maybe (GType l, GTBindVar l, GType l)
takeTExists (TExists k b t)     = Just (k, b, t)
takeTExists _                   = Nothing

