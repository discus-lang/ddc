
-- | Given a term containing some store locations, a closure is an upper 
--   bound on the regions those locations are in.
module DDC.Type.Operators.Trim 
        (trimClosure)
where
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Collect.Free
import DDC.Type.Env             (Env)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- | Trim a closure.
trimClosure :: Ord n => Closure n -> Closure n
trimClosure cc
        = TSum $ trimToSumC Env.empty cc


-- | Trim a closure down to a closure sum.
trimToSumC :: forall n. (Free n (TypeSum n), Ord n) => Env n -> Closure n -> TypeSum n
trimToSumC env cc
 = case cc of
        -- Locally bound closure variables can't be instantiated to anything else.
        TVar u
         | Env.member u env     -> Sum.empty kClosure
         | otherwise            -> Sum.singleton kClosure cc
        
        -- There aren't any locally bound closure constructors, 
        --  but just return the provided type to keep 'trimC' total.
        TCon{}
         -> Sum.singleton kClosure cc
        
        -- Add locally bound variable to the environment.
        -- TODO: drop forall if there are no free vars in the body, 
        --       will also have to lower locally bound debruijn indices.
        -- TODO: for now we just drop the forall if the free vars list is empty,
        --       which is a safe approx. 
        -- TODO: recomputing the free vars like this is bad for complexity, 
        --       should track free vars on the fly.
        TForall b t
         -> let c'      = trimToSumC (Env.extend b env) t       :: TypeSum n
                ns      = free Env.empty c'                     :: Set (Bound n)
            in  if Set.size ns == 0
                then Sum.empty kClosure
                else Sum.singleton kClosure $ TForall b $ TSum c'

        -- Share constructor applied to a region.
        TApp (TCon (TyConComp TcConShare)) _
         -> Sum.singleton kClosure cc
        
        -- DeepShare constructor applied to a data type.
        TApp (TCon (TyConComp TcConDeepShare)) t2 
         -> trimDeepSharedD env t2

        -- Some other constructor we don't know about.
        TApp{}
         -> Sum.singleton kClosure cc

        -- Trim components of a closure sum and rebuild the sum.
        TSum ts
         -> Sum.fromList kClosure 
          $ concatMap (Sum.toList . trimToSumC env)
          $ Sum.toList ts
        

-- | Trim the argument of a DeepShared constructor down to a closure sum.
--   The argument is of data kind.
trimDeepSharedD :: forall n. (Free n (TypeSum n), Ord n) => Env n -> Type n -> TypeSum n
trimDeepSharedD env tt
 = case tt of
        -- Locally bound variables can never be instantiated,
        --  so can't be made to contain region variables.
        TVar u
         | Env.member u env     -> Sum.empty kClosure
         | otherwise            -> Sum.singleton kClosure $ tDeepShare tt

        -- Naked constructors don't contain region variables.
        --  This handles closure terms like 'DeepShare Unit'.
        TCon{}
         -> Sum.empty kClosure

        -- Add locally bound variable to the environment.
        -- TODO: for now we just drop the forall if the free vars list is empty,
        --       but also need to lower debruijn indices.
        -- TODO: drop forall if there are no free vars in the body, 
        --       will also have to lower locally bound debruijn indices.
        -- TODO: recomputing the free vars like this is bad for complexity, 
        --       should track free vars on the fly.
        TForall b t
         -> let c'      = trimDeepSharedD (Env.extend b env) t  :: TypeSum n
                ns      = free Env.empty c'                     :: Set (Bound n)
            in  if Set.size ns == 0
                then Sum.empty kClosure
                else Sum.singleton kClosure $ TForall b $ TSum c'

        -- 
        TApp{}
         -- For functions, all params are immaterial except the closure.
         |  Just (_t1, _eff, clo, _t2)      <- takeTFun tt
         -> trimToSumC env clo

         |  otherwise
         -> Sum.singleton kClosure $ tDeepShare tt

        -- We shouldn't get sums of data types in regular code, 
        --  but the (tBot kData) form might appear in debugging. 
        TSum{}
         -> Sum.singleton kClosure $ tDeepShare tt


