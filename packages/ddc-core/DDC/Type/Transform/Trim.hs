
-- | Given a term containing some store locations, a closure is an upper 
--   bound on the regions those locations are in.
module DDC.Type.Transform.Trim 
        (trimClosure)
where
import DDC.Core.Collect
import DDC.Type.Check.CheckCon
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import Control.Monad
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


-- | Trim a closure.
--
--   This function assumes the closure is well-kinded, 
--   and may return Nothing if this is not the case.
trimClosure 
        :: Ord n
        => Closure n 
        -> Maybe (Closure n)

trimClosure cc
        = liftM TSum $ trimToSumC cc


-- | Trim a closure down to a closure sum.
--   May return 'Nothing' if the closure is mis-kinded.
trimToSumC 
        :: forall n. Ord n
        => Closure n -> Maybe (TypeSum n)

trimToSumC cc
 = case cc of
        -- Keep closure variables.
        TVar{}          -> Just $ Sum.singleton kClosure cc

        -- There aren't any naked constructors of closure type.
        -- If we find a constructor the closure is miskinded.
        TCon{}          -> Nothing
        
        -- The body of a forall should have data or witness kind.
        -- If we find a forall then the closure is miskinded.
        TForall{}       -> Nothing

        -- Keep use constructor applied to a region.
        TApp (TCon (TyConSpec TcConUse)) _
         -> Just $ Sum.singleton kClosure cc
        
        -- Trim DeepUse constructor applied to a data type.
        TApp (TCon (TyConSpec TcConDeepUse)) t2 
         -> Just $ trimDeepUsedD t2

        -- Some other constructor we don't know about,
        --  perhaps using a type variable of higher kind.
        TApp{}          -> Just $ Sum.singleton kClosure cc

        -- Trim components of a closure sum and rebuild the sum.
        TSum ts
         -> case sequence $ map trimToSumC $ Sum.toList ts of
                Nothing         -> Nothing
                Just sums       -> Just $ Sum.fromList kClosure
                                $  concatMap Sum.toList sums


-- | Trim the argument of a DeepUsed constructor down to a closure sum.
--   The argument is of data kind.
trimDeepUsedD 
        :: forall n. Ord n
        => Type n -> TypeSum n

trimDeepUsedD tt
 = case tt of
        -- Keep type variables.
        TVar{}          -> Sum.singleton kClosure $ tDeepUse tt

        -- Naked data constructors like 'Unit' don't contain region variables,
        --  but the interpreter uses constructors of region kind to encode
        --  region handes, that we need to keep.
        TCon tc
         |  Just k       <- takeKindOfTyCon tc
         ,  isRegionKind k
         -> Sum.singleton kClosure $ tDeepUse tt

         | otherwise
         -> Sum.empty kClosure

        -- Add locally bound variable to the environment.
        -- See Note: Trimming Foralls. 
        TForall{}
         -> let ns      = freeT Env.empty tt  :: Set (Bound n)
            in  if Set.size ns == 0
                 then Sum.empty kClosure
                 else Sum.singleton kClosure $ tDeepUse tt

        -- Trim function constructors.
        -- See Note: Material variables and the interpreter
        TApp (TApp (TApp (TApp (TCon (TyConSpec TcConFun)) _t1) _eff) clo) _t2
         -> Sum.singleton kClosure clo

        -- Trim a type application.
        -- See Note: Trimming with higher kinded type vars.
        TApp{}
         -> case takeTyConApps tt of
             Just (tc, args)     
              | Just k          <- takeKindOfTyCon tc
              , Just cs         <- sequence $ zipWith makeUsed (takeKFuns' k) args
              ->  Sum.fromList kClosure cs

             _ -> Sum.singleton kClosure $ tDeepUse tt

        -- We shouldn't get sums of data types in regular code, 
        --  but the (tBot kData) form might appear in debugging. 
        TSum{}          -> Sum.singleton kClosure $ tDeepUse tt


-- | Make the appropriate Use term for a type of the given kind, or `Nothing` if
--  there isn't one. Also recursively trim types of data kind.
makeUsed :: (Eq n, Ord n) => Kind n -> Type n -> Maybe (Closure n)
makeUsed k t
        | isRegionKind k        = Just $ tUse t
        | isDataKind   k        = Just $ TSum $ trimDeepUsedD t
        | isEffectKind k        = Just $ tBot kClosure
        | isClosureKind k       = Just $ t
        | otherwise             = Nothing 


{- [Note: Trimming with higher kinded type vars]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   We can't just look at the free variables here and wrap Use and DeepUse constructors
   around them, as the type may contain higher kinded type variables such as: (t a).
   We cannot simply drop such variables, as they may be substituted for types that
   contain components that we must keep in the closure. To handle this, when we see
   higher kinded type varibles we preserve the entire type application, which is
   DeepUse (t a) in this example.

   [Note: Trimming Foralls]
   ~~~~~~~~~~~~~~~~~~~~~~~~
   For now we just drop the forall if the free vars list is empty. This is ok because
   we only do this at top-level, so don't need to lower debruijn indices to account for
   deleted intermediate quantifiers.

   [Note: Material variables and the interpreter]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Even though we're not tracking material vars properly yet, 
   for the interpreter we need to ignore the non-material parameters of the
   function constructor so that we can treat store location constructors as
   having an empty closure. For example:

    L2# :: Int R1# -> Int R1#
   
   This does not capture the R1# region, even the handle for it is in its type.
-}

