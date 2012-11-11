
module DDC.Type.Equiv
        ( equivT
        , equivWithBindsT)
where
import DDC.Type.Transform.Crush
import DDC.Type.Compounds
import DDC.Type.Bind
import DDC.Type.Exp
import qualified DDC.Type.Sum   as Sum


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
 = let  t1'     = unpackSumT $ crushSomeT t1
        t2'     = unpackSumT $ crushSomeT t2
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
         -> tc1 == tc2

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
unpackSumT tt			 = tt


