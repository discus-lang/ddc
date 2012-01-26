module DDC.Type.Transform.Crush
        (crushT)
where
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Exp
import qualified DDC.Type.Sum   as Sum
import DDC.Type.Pretty


-- | Crush compound effect terms into their components.
crushT :: (Ord n, Pretty n) => Type n -> Type n
crushT tt
 = case tt of
        TVar{}          -> tt
        TCon{}          -> tt
        TForall b t
         -> TForall b (crushT t)

        TSum ts         
         -> TSum
          $ Sum.fromList (Sum.kindOfSum ts)   
          $ map crushT 
          $ Sum.toList ts

        TApp t1 t2
         -- Head Read.
         |  Just (TyConComp TcConHeadRead, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of

             -- Type has a head region.
             Just (TyConBound u, (tR : _)) 
              |  (k1 : _, _) <- takeKFuns (typeOfBound u)
              ,  isRegionKind k1
              -> tRead tR

             -- Type has no head region.
             -- This happens with  case () of { ... }
             Just (TyConBound _, [])        -> tBot kEffect

             _ -> tt

         -- Deep Read.
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConComp TcConDeepRead, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound u, ts)
              | (ks, _)  <- takeKFuns (typeOfBound u)
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepRead ks ts
              -> crushT $ TSum $ Sum.fromList kEffect effs

             _ -> tt

         -- Deep Write
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConComp TcConDeepWrite, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound u, ts)
              | (ks, _)  <- takeKFuns (typeOfBound u)
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepWrite ks ts
              -> crushT $ TSum $ Sum.fromList kEffect effs

             _ -> tt 

         -- Deep Alloc
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConComp TcConDeepAlloc, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound u, ts)
              | (ks, _)  <- takeKFuns (typeOfBound u)
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepAlloc ks ts
              -> crushT $ TSum $ Sum.fromList kEffect effs

             _ -> tt


         -- Deep Global
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConWitness TwConDeepGlobal, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound u, ts)
              | (ks, _)  <- takeKFuns (typeOfBound u)
              , length ks == length ts
              , Just props       <- sequence $ zipWith makeDeepGlobal ks ts
              -> crushT $ TSum $ Sum.fromList kWitness props

             _ -> tt 

         | otherwise
         -> TApp (crushT t1) (crushT t2)


-- | If this type has first order kind then wrap with the 
--   appropriate read effect.
makeDeepRead :: Kind n -> Type n -> Maybe (Effect n)
makeDeepRead k t
        | isRegionKind  k       = Just $ tRead t
        | isDataKind    k       = Just $ tDeepRead t
        | isClosureKind k       = Just $ tBot kEffect
        | isEffectKind  k       = Just $ tBot kEffect
        | otherwise             = Nothing


-- | If this type has first order kind then wrap with the 
--   appropriate read effect.
makeDeepWrite :: Kind n -> Type n -> Maybe (Effect n)
makeDeepWrite k t
        | isRegionKind  k       = Just $ tWrite t
        | isDataKind    k       = Just $ tDeepWrite t
        | isClosureKind k       = Just $ tBot kEffect
        | isEffectKind  k       = Just $ tBot kEffect
        | otherwise             = Nothing


-- | If this type has first order kind then wrap with the 
--   appropriate read effect.
makeDeepAlloc :: Kind n -> Type n -> Maybe (Effect n)
makeDeepAlloc k t
        | isRegionKind  k       = Just $ tAlloc t
        | isDataKind    k       = Just $ tDeepAlloc t
        | isClosureKind k       = Just $ tBot kEffect
        | isEffectKind  k       = Just $ tBot kEffect
        | otherwise             = Nothing


-- | If this type has first order kind then wrap with the 
--   appropriate read effect.
makeDeepGlobal :: Kind n -> Type n -> Maybe (Type n)
makeDeepGlobal k t
        | isRegionKind  k       = Just $ tGlobal t
        | isDataKind    k       = Just $ tDeepGlobal t
        | isClosureKind k       = Nothing
        | isEffectKind  k       = Just $ tBot kEffect
        | otherwise             = Nothing


{- [Note: Crushing with higher kinded type vars]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We can't just look at the free variables here and wrap Read and DeepRead constructors
   around them, as the type may contain higher kinded type variables such as: (t a).
   Instead, we'll only crush the effect when all variable have first-order kind.
   When comparing types with higher order variables, we'll have to use the type
   equivalence checker, instead of relying on the effects to be pre-crushed.
-}
