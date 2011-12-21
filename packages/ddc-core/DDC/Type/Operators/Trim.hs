
-- | Given a term containing some store locations, a closure is an upper 
--   bound on the regions those locations are in.
module DDC.Type.Operators.Trim 
        (trimClosure)
where
import DDC.Type.Check.CheckCon
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Collect.Free
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- | Trim a closure.
trimClosure :: Ord n => Closure n -> Closure n
trimClosure cc
        = TSum $ trimToSumC cc


-- | Trim a closure down to a closure sum.
--   This can `error` if the closure is mis-kinded.
trimToSumC :: forall n. (Free n (TypeSum n), Ord n) => Closure n -> TypeSum n
trimToSumC cc
 = case cc of
        -- Keep closure variables.
        TVar{}          -> Sum.singleton kClosure cc

        -- There aren't any naked constructors of closure type.
        TCon{}          -> error "trimToSumC: found naked closure constructor"
        
        -- The body of a forall should have data kind.                          -- TODO: enforce this in kinding rules.
        TForall{}       -> error "trimToSumC: found forall"

        -- Keep use constructor applied to a region.
        TApp (TCon (TyConComp TcConUse)) _
         -> Sum.singleton kClosure cc
        
        -- Trim DeepUse constructor applied to a data type.
        TApp (TCon (TyConComp TcConDeepUse)) t2 
         -> trimDeepUsedD t2

        -- Some other constructor we don't know about,
        --  perhaps using a type variable of higher kind.
        TApp{}          -> Sum.singleton kClosure cc

        -- Trim components of a closure sum and rebuild the sum.
        TSum ts
         -> Sum.fromList kClosure 
          $ concatMap (Sum.toList . trimToSumC)
          $ Sum.toList ts
        

-- | Trim the argument of a DeepUsed constructor down to a closure sum.
--   The argument is of data kind.
---
--   NOTE: We can't just look at the free variables here and wrap Use
--   and DeepUse constructors around them, as the type may contain higher
--   kinded type variables such as: (t a). In this case we must preserve
--   the DeepUse constructor and return DeepUse (t a).
--
trimDeepUsedD :: forall n. (Free n (TypeSum n), Ord n) => Type n -> TypeSum n
trimDeepUsedD tt
 = case tt of
        -- Keep type variables.
        TVar{}          -> Sum.singleton kClosure $ tDeepUse tt

        -- Naked data constructors don't contain region variables.
        --  This handles closure terms like 'DeepUse Unit'.
        TCon{}          -> Sum.singleton kClosure $ tDeepUse tt

        -- Add locally bound variable to the environment.
        -- TODO: For now we just drop the forall if the free vars list is empty.
        --       This is ok because we only do this at top-level, so don't need
        --       to lower debruijn indices to account for deleted intermediate
        --       quantifiers.
        TForall{}
         -> let ns      = free Env.empty tt  :: Set (Bound n)
            in  if Set.size ns == 0
                 then Sum.empty kClosure
                 else Sum.singleton kClosure $ tDeepUse tt

        -- 
        TApp{}
         -> case takeTyConApps tt of
             Just (tc, args)     
              | Just k          <- takeKindOfTyCon tc
              , Just cs         <- sequence $ zipWith makeUsed (takeKFuns k) args
              -> Sum.fromList kClosure cs

             _ ->  Sum.singleton kClosure $ tDeepUse tt


        -- We shouldn't get sums of data types in regular code, 
        --  but the (tBot kData) form might appear in debugging. 
        TSum{}          -> Sum.singleton kClosure $ tDeepUse tt


-- | Make the appropriate Use term for a type of the given kind,
--   or `Nothing` if there isn't one.
makeUsed :: Kind n -> Type n -> Maybe (Closure n)
makeUsed k t
        | isRegionKind k        = Just $ tUse t
        | isDataKind   k        = Just $ tDeepUse t                     -- TODO: re-trim args when we have data types.
        | isEffectKind k        = Just $ tEmpty kClosure
        | isClosureKind k       = Just $ t
        | otherwise             = Nothing

