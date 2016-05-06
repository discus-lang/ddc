{-# OPTIONS -fno-warn-missing-signatures #-}
module DDC.Type.Exp.Generic.Compounds
        ( -- * Type Applications
          makeTApps,    takeTApps

          -- * Function Types
        , makeTFun,     makeTFuns
        , takeTFun,     takeTFuns,      takeTFuns'

          -- * Forall Types
        , makeTForall,  takeTForall

          -- * Exists Types
        , makeTExists,  takeTExists)
where
import DDC.Type.Exp.Generic.Exp
import DDC.Type.Exp.Generic.Binding


-- Type Applications ----------------------------------------------------------
-- | Construct a sequence of type applications.
makeTApps :: GType l -> [GType l] -> GType l
makeTApps t1 ts     = foldl TApp t1 ts


-- | Flatten a sequence of type applications into the function part and
--   arguments, if any.
takeTApps :: GType l -> [GType l]
takeTApps tt
 = case tt of
        TApp t1 t2 -> takeTApps t1 ++ [t2]
        _          -> [tt]


-- Function Types -------------------------------------------------------------
-- | Construct a function type with the given parameter and result type.
makeTFun :: GType l -> GType l -> GType l
makeTFun t1 t2           = (TFun `TApp` t1) `TApp` t2
infixr `makeTFun`


-- | Like `makeFun` but taking a list of parameter types.
makeTFuns :: [GType l] -> GType l -> GType l
makeTFuns []     t1      = t1
makeTFuns (t:ts) t1      = t `makeTFun` makeTFuns ts t1


-- | Destruct a function type into its parameter and result types,
--   returning `Nothing` if this isn't a function type.
takeTFun :: GType l -> Maybe (GType l, GType l)
takeTFun tt
 = case tt of
        TApp (TApp (TCon TyConFun) t1) t2 
                -> Just (t1, t2)
        _       -> Nothing


-- | Destruct a function type into into all its parameters and result type,
--   returning an empty parameter list if this isn't a function type.
takeTFuns :: GType l -> ([GType l], GType l)
takeTFuns tt
 = case tt of
        TApp (TApp (TCon TyConFun) t1) t2
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
makeTForall :: Anon l => l -> GType l -> (GType l -> GType l) -> GType l
makeTForall l k makeBody
 =  withBinding l $ \b u -> TForall k b (makeBody (TVar u))


-- | Destruct a forall quantified type, if this is one.
takeTForall :: GType l -> Maybe (GType l, GBind l, GType l)
takeTForall (TForall k b t)     = Just (k, b, t)
takeTForall _                   = Nothing


-- Exists types ---------------------------------------------------------------
-- | Construct an exists quantified type using an anonymous binder.
makeTExists :: Anon l => l -> GType l -> (GType l -> GType l) -> GType l
makeTExists l k makeBody
 =  withBinding l $ \b u -> TExists k b (makeBody (TVar u))


-- | Destruct an exists quantified type, if this is one.
takeTExists :: GType l -> Maybe (GType l, GBind l, GType l)
takeTExists (TExists k b t)     = Just (k, b, t)
takeTExists _                   = Nothing

