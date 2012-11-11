-- | Check whether two effects are non-interfering
module DDC.Core.Transform.Rewrite.Disjoint
        ( checkDisjoint
        , checkDistinct )
where
import DDC.Core.Exp
import DDC.Type.Predicates
import DDC.Type.Compounds
import qualified DDC.Core.Transform.Rewrite.Env	as RE
import qualified DDC.Type.Sum			as Sum
import qualified DDC.Type.Transform.Crush	as TC


-- | Check whether a disjointness property is true in the given
--   rewrite environment.
--
--   Disjointness means that two effects do not interfere.
--
--   Context is important because if two regions are known to be
--   distinct, reading from one and writing to another is valid.
--   If they have different names they may not be distinct.
--
--   All read effects are disjoint with other reads.
--
-- > Disjoint (Read r1) (Read r2)
-- > Disjoint (Read r1) (DeepRead a)
--
--   Allocation effects are disjoint with everything.
--
-- > Disjoint (Alloc r) (_)
--
--   Atomic reads and write effects are disjoint if they are to distinct regions.
--
-- >         Distinct r1 r2
-- > -----------------------------
-- > Disjoint (Read r1) (Write r2)
-- 
--   @DeepWrite@ effects are only disjoint with allocation effects, because
--   we don't know what regions it will write to.
--
--   An effect sum is disjoint from some other effect if all its components are.
--
-- > Disjoint f1 g /\ Disjoint f2 g
-- > -----------------------------
-- >      Disjoint (f1 + f2) g
--
--   Disjointness is commutative.
--
-- > Disjoint f g
-- > ------------
-- > Disjoint g f
--  
--   Example:
--   
-- >  checkDisjoint
-- >	(Disjoint (Read r1 + Read r2) (Write r3))
-- >	[Distinct r1 r3, Distinct r2 r3]
-- >  = True
--
checkDisjoint
        :: (Ord n, Show n)
        => Type n               -- ^ Type of property we want
                                --   eg @Disjoint e1 e2@
        -> RE.RewriteEnv a n	-- ^ Environment we're rewriting in.
        -> Bool

checkDisjoint c env
        -- The type must have the form "Disjoint e1 e2"
        | [TCon (TyConWitness TwConDisjoint), fs, gs] <- takeTApps c
        = and [ areDisjoint env g f 
                | f <- sumList $ TC.crushEffect fs
                , g <- sumList $ TC.crushEffect gs ]

        | otherwise
        = False
        where   sumList (TSum ts) = Sum.toList ts
                sumList tt        = [tt]


-- | Check whether two atomic effects are disjoint.
areDisjoint 
        :: (Ord n, Show n)
        => RE.RewriteEnv a n
        -> Effect n
        -> Effect n
        -> Bool

areDisjoint env t1 t2
        -- Allocations are disjoint with everything.
        |   isSomeAllocEffect t1
         || isSomeAllocEffect t2
        = True 

        -- All the read effects are disjoint with each other.
        | isSomeReadEffect t1
        , isSomeReadEffect t2
        = True

        -- Combinations of reads and writes are disjoint
        -- if the regions are distinct.
        | TApp _ tR1            <- t1
        , TApp _ tR2            <- t2
        ,   (isReadEffect  t1 && isWriteEffect t2)
         || (isWriteEffect t1 && isReadEffect  t2)
         || (isWriteEffect t1 && isWriteEffect t2)
        = areDistinct env tR1 tR2

        -- All other effects are assumed to be interfering.
        | otherwise                     = False


-- Distinct -------------------------------------------------------------------
-- | Check whether a distintness property is true in the given 
--   rewrite environment.
--
--   Distinctness means that two regions do not alias.
--
checkDistinct
    :: Ord n
    => Type n			-- ^ Type of the property we want,
                                --   eg @Distinct r1 r2@
    -> RE.RewriteEnv a n	-- ^ Environment we're rewriting in.
    -> Bool

checkDistinct c env
        -- It's of the form "Distinct r q"
        | (TCon (TyConWitness (TwConDistinct _)) : args)
        	<- takeTApps c
        = all (uncurry $ areDistinct env) (combinations args)

        | otherwise
        = False

        where   combinations [] = []
                combinations (x:xs) = repeat x `zip` xs ++ combinations xs


-- | Check if two regions are distinct.
areDistinct
        :: Ord n
        => RE.RewriteEnv a n
        -> Type n -> Type n -> Bool

areDistinct env t1 t2
        | Just u1   <- takeBound t1
        , Just u2   <- takeBound t2
        = areDistinctBound env u1 u2

        | otherwise
        = False
        where   takeBound (TVar u)                = Just u
                takeBound (TCon (TyConBound u _)) = Just u
                takeBound _                       = Nothing


-- | Check whether two regions are distinct.
--   This version takes `Bounds` so we don't need to worry about
--   region constructors like R0# directly.
areDistinctBound 
        :: Ord n
        => RE.RewriteEnv a n
        -> Bound n -> Bound n -> Bool

areDistinctBound env p q
        -- If they are the same, they can't possibly be different
        | p == q
        = False

        -- If they're both named primitives (eg R0#, R1#)
        -- we can just check name-equality, since can't be bound in lambdas
        -- Or if they're both bound in letregions, we can check by name
        -- (and we know names are different because that's an insta-fail)
        | concrete p && concrete q
        = True

        -- Check witness map for "Distinct p q" and vice versa
        | any check $ RE.getWitnesses env
        = True

        -- Otherwise not.
        | otherwise
        = False

        where   -- Check if region is 'concrete' either a region handle (R0#) 
                -- or bound by a letregion in a higher scope.
                concrete r
                 = case r of
                        UPrim _ _ -> True
                        _	  -> RE.containsRegion r env

                check w
                 | (TCon (TyConWitness (TwConDistinct _)) : args)
                        <- takeTApps w
                 = rgn p `elem` args && rgn q `elem` args

                 | otherwise
                 = False

                rgn b
                 = case b of
                    UPrim _ t   -> TCon (TyConBound b t)
                    _	        -> TVar b

