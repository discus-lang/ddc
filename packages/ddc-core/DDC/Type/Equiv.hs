
module DDC.Type.Equiv
        ( equivT
        , equivWithBindsT
        , equivTyCon

        , crushSomeT
        , crushEffect)
where
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Bind
import DDC.Type.Exp
import DDC.Type.Env             (TypeEnv)
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
-- | Check equivalence of types.
--
--   Checks equivalence up to alpha-renaming, as well as crushing of effects
--   and trimming of closures.
--  
--   * Return `False` if we find any free variables.
--
--   * We assume the types are well-kinded, so that the type annotations on
--     bound variables match the binders. If this is not the case then you get
--     an indeterminate result.
--
equivT  :: Ord n => Type n -> Type n -> Bool
equivT t1 t2
        = equivWithBindsT [] [] t1 t2

-- | Like `equivT` but take the initial stacks of type binders.
equivWithBindsT
        :: Ord n
        => [Bind n]
        -> [Bind n]
        -> Type n
        -> Type n
        -> Bool

equivWithBindsT stack1 stack2 t1 t2
 = let  t1'     = unpackSumT $ crushSomeT Env.empty t1
        t2'     = unpackSumT $ crushSomeT Env.empty t2

   in case (t1', t2') of
        (TVar u1,         TVar u2)
         -- Free variables are name-equivalent, bound variables aren't:
         -- (forall a. a) != (forall b. a)
         | Nothing      <- getBindType stack1 u1
         , Nothing      <- getBindType stack2 u2
         , u1 == u2     -> checkBounds u1 u2 True

         -- Both variables are bound in foralls, so check the stack
         -- to see if they would be equivalent if we named them.
         | Just (ix1, t1a)   <- getBindType stack1 u1
         , Just (ix2, t2a)   <- getBindType stack2 u2
         , ix1 == ix2
         -> checkBounds u1 u2 
         $  equivWithBindsT stack1 stack2 t1a t2a

         | otherwise
         -> checkBounds u1 u2
         $  False

        -- Constructor names must be equal.
        (TCon tc1,        TCon tc2)
         -> equivTyCon tc1 tc2

        -- Push binders on the stack as we enter foralls.
        (TForall b11 t12, TForall b21 t22)
         |  equivT  (typeOfBind b11) (typeOfBind b21)
         -> equivWithBindsT
                (b11 : stack1)
                (b21 : stack2)
                t12 t22

        -- Decend into applications.
        (TApp t11 t12,    TApp t21 t22)
         -> equivWithBindsT stack1 stack2 t11 t21
         && equivWithBindsT stack1 stack2 t12 t22
        
        -- Sums are equivalent if all of their components are.
        (TSum ts1,        TSum ts2)
         -> let ts1'      = Sum.toList ts1
                ts2'      = Sum.toList ts2

                -- If all the components of the sum were in the element
                -- arrays then they come out of Sum.toList sorted
                -- and we can compare corresponding pairs.
                checkFast = and $ zipWith (equivWithBindsT stack1 stack2) ts1' ts2'

                -- If any of the components use a higher kinded type variable
                -- like (c : % ~> !) then they won't nessesarally be sorted,
                -- so we need to do this slower O(n^2) check.
                -- Make sure to get the bind stacks the right way around here.
                checkSlow = and [ or (map (equivWithBindsT stack1 stack2 t1c) ts2') 
                                | t1c <- ts1' ]
                         && and [ or (map (equivWithBindsT stack2 stack1 t2c) ts1') 
                                | t2c <- ts2' ]

            in  (length ts1' == length ts2')
            &&  (checkFast || checkSlow)

        (_, _)  -> False


-- | If we have a UName and UPrim with the same name then these won't match
--   even though they pretty print the same. This will only happen due to 
--   a compiler bugs, but is very confusing when it does, so we check for
--   this case explicitly.
checkBounds :: Eq n => Bound n -> Bound n -> a -> a
checkBounds u1 u2 x
 = case (u1, u2) of
        (UName n2, UPrim n1 _)
         | n1 == n2     -> die

        (UPrim n1 _, UName n2)
         | n1 == n2     -> die

        _               -> x
 where
  die   = error $ unlines
        [ "DDC.Type.Equiv"
        , "  Found a primitive and non-primitive bound variable with the same name."]


-- | Unpack single element sums into plain types.
unpackSumT :: Type n -> Type n
unpackSumT (TSum ts)
        | [t]   <- Sum.toList ts = t
unpackSumT tt                    = tt


-- TyCon 
-- | Check if two `TyCons` are equivalent.
--   We need to handle `TyConBound` specially incase it's kind isn't attached,
equivTyCon :: Eq n => TyCon n -> TyCon n -> Bool
equivTyCon tc1 tc2
 = case (tc1, tc2) of  
        (TyConBound u1 _, TyConBound u2 _) -> u1  == u2
        _                                  -> tc1 == tc2



---------------------------------------------------------------------------------------------------
-- | Crush compound effects and closure terms.
--   We check for a crushable term before calling crushT because that function
--   will recursively crush the components. 
--   As equivT is already recursive, we don't want a doubly-recursive function
--   that tries to re-crush the same non-crushable type over and over.
--
crushSomeT :: Ord n => TypeEnv n -> Type n -> Type n
crushSomeT caps tt
 = {-# SCC crushSomeT #-}
   case tt of
        TApp (TCon tc) _
         -> case tc of
                TyConSpec    TcConDeepRead   -> crushEffect caps tt
                TyConSpec    TcConDeepWrite  -> crushEffect caps tt
                TyConSpec    TcConDeepAlloc  -> crushEffect caps tt
                _                            -> tt

        _ -> tt


-- | Crush compound effect terms into their components.
--
--   For example, crushing @DeepRead (List r1 (Int r2))@ yields @(Read r1 + Read r2)@.
--
crushEffect 
        :: Ord n 
        => TypeEnv n            -- ^ Globally available capabilities.
        -> Effect n             -- ^ Type to crush. 
        -> Effect n

crushEffect caps tt
 = {-# SCC crushEffect #-}
   case tt of
        TVar{}          -> tt
        TCon{}          -> tt

        TForall b t
         -> TForall b $ crushEffect caps t

        TSum ts         
         -> TSum
          $ Sum.fromList (Sum.kindOfSum ts)   
          $ map (crushEffect caps)
          $ Sum.toList ts

        TApp{}
         |  or [equivT tt t | (_, t) <- Map.toList $ Env.envMap caps]
         -> tSum kEffect []

        TApp t1 t2
         -- Head Read.
         |  Just (TyConSpec TcConHeadRead, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of

             -- Type has a head region.
             Just (TyConBound _ k, (tR : _)) 
              |  (k1 : _, _) <- takeKFuns k
              ,  isRegionKind k1
              -> tRead tR

             -- Type has no head region.
             -- This happens with  case () of { ... }
             Just (TyConSpec  TcConUnit, [])
              -> tBot kEffect

             Just (TyConBound _ _,       _)     
              -> tBot kEffect

             _ -> tt

         -- Deep Read.
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConSpec TcConDeepRead, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound _ k, ts)
              | (ks, _)  <- takeKFuns k
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepRead ks ts
              -> crushEffect caps $ TSum $ Sum.fromList kEffect effs

             _ -> tt

         -- Deep Write
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConSpec TcConDeepWrite, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound _ k, ts)
              | (ks, _)  <- takeKFuns k
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepWrite ks ts
              -> crushEffect caps $ TSum $ Sum.fromList kEffect effs

             _ -> tt 

         -- Deep Alloc
         -- See Note: Crushing with higher kinded type vars.
         | Just (TyConSpec TcConDeepAlloc, [t]) <- takeTyConApps tt
         -> case takeTyConApps t of
             Just (TyConBound _ k, ts)
              | (ks, _)  <- takeKFuns k
              , length ks == length ts
              , Just effs       <- sequence $ zipWith makeDeepAlloc ks ts
              -> crushEffect caps $ TSum $ Sum.fromList kEffect effs

             _ -> tt


         | otherwise
         -> TApp (crushEffect caps t1) (crushEffect caps t2)


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



{- [Note: Crushing with higher kinded type vars]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We can't just look at the free variables here and wrap Read and DeepRead constructors
   around them, as the type may contain higher kinded type variables such as: (t a).
   Instead, we'll only crush the effect when all variable have first-order kind.
   When comparing types with higher order variables, we'll have to use the type
   equivalence checker, instead of relying on the effects to be pre-crushed.
-}
