
-- | Given a term containing some store locations, a closure is an upper 
--   bound on the regions those locations are in.
module DDC.Type.Operators.Trim 
        (trimClosure)
where
import DDC.Type.Exp
import DDC.Type.Compounds
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
        -- Locally bound closure variables can't be instantiated to anything else.
        TVar{}          -> Sum.singleton kClosure cc

        -- There aren't any naked constructors of closure type.
        TCon{}          -> error "trimToSumC: found naked closure constructor"
        
        -- The body of a forall should have data kind.                          -- TODO: enforce this in kinding rules.
        TForall{}       -> error "trimToSumC: found forall"

        -- Use constructor applied to a region.
        TApp (TCon (TyConComp TcConUse)) _
         -> Sum.singleton kClosure cc
        
        -- DeepUse constructor applied to a data type.
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
        TCon{}          -> Sum.empty kClosure

        -- Add locally bound variable to the environment.
        -- TODO: for now we just drop the forall if the free vars list is empty,
        --       but also need to lower debruijn indices.
        -- TODO: drop forall if there are no free vars in the body, 
        --       will also have to lower locally bound debruijn indices.
        -- TODO: recomputing the free vars like this is bad for complexity, 
        --       should track free vars on the fly.
        TForall{}
         -> let ns      = free Env.empty tt  :: Set (Bound n)
            in  if Set.size ns == 0
                then Sum.empty kClosure
                else Sum.singleton kClosure $ tDeepUse tt

        TApp{}          -> Sum.singleton kClosure $ tDeepUse tt

        -- We shouldn't get sums of data types in regular code, 
        --  but the (tBot kData) form might appear in debugging. 
        TSum{}          -> Sum.singleton kClosure $ tDeepUse tt


