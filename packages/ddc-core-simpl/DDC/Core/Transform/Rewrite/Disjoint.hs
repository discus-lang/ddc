-- | Check whether two effects are non-interfering
module DDC.Core.Transform.Rewrite.Disjoint
    ( checkDisjoint )
where

import DDC.Core.Exp
import qualified DDC.Core.Transform.Rewrite.Env	as RE
import qualified DDC.Type.Compounds		as T
import qualified DDC.Type.Exp			as T
import qualified DDC.Type.Sum			as TS
import qualified DDC.Type.Transform.Crush	as TC

-- | Check whether two effects are non-interfering.
--   Context is important because if two regions are known to be
--   distinct, reading from one and writing to another is valid.
--   If they have different names they may not be distinct.
--
--   Reads are safe:
--	Disjoint (Read r1) (Read r2)
--	Disjoint (Read r1) (DeepRead a)
--
--   Allocations are always safe:
--      Disjoint (Alloc r) (_)
--
--   Writes are only safe if the same region is not read or written:
--	        Distinct r1 r2
--	-----------------------------
--	Disjoint (Read r1) (Write r2)
--
--	        Distinct r1 r2
--	------------------------------
--	Disjoint (Write r1) (Write r2)
--
--   But DeepWrite etc can't mix with anything except Alloc,
--   because we don't even know what regions it is.
--
--   It is commutative:
--     Disjoint f g
--     ------------
--     Disjoint g f
--
--   Effect sums are OK if all their elements are disjoint:
--     Disjoint f1 g /\ Disjoint f2 g
--     -----------------------------
--           Disjoint (f1+f2) g
--   
--
--   checkDisjoint
--	(Disjoint (Read r1 + Read r2) (Write r3))
--	[Distinct r1 r3, Distinct r2 r3]
--   = True
--
checkDisjoint
    :: (Eq n, Ord n, Show n)
    => Type n			-- ^ Target, eg "Disjoint f g"
    -> RE.RewriteEnv a n	-- ^ Environment: distinctness map
    -> Bool
checkDisjoint c env
 -- It's of the form "Disjoint f g"
 | [TCon (TyConWitness TwConDisjoint), fs, gs]
		<- T.takeTApps c
 = let
      fs'	=  map T.takeTApps $ sumList $ TC.crushEffect fs
      gs'	=  map T.takeTApps $ sumList $ TC.crushEffect gs
   in and [ disjoint f g && disjoint g f | f <- fs', g <- gs' ]

 | otherwise
 = False
 where
    sumList (TSum ts) = TS.toList ts
    sumList tt	      = [tt]

    -- Anything goes with allocations
    disjoint _ (TCon (TyConSpec TcConAlloc) : _)
     = True
    disjoint _ (TCon (TyConSpec TcConDeepAlloc) : _)
     = True

    -- Check a variable. Since we have no idea what it may end up being,
    -- assume the worst. It can only work if the right-hand is an Alloc
    disjoint (TVar _ : _) _
     = False

    -- Check a builtin effect. If it's a read or alloc, assume rhs is OK
    -- (other direction will be checked later).
    -- If it's a write, check which regions and if it overlaps with rhs.
    disjoint (TCon (TyConSpec fcon) : fargs) (TCon (TyConSpec gcon) : gargs)
     = case fcon of
       TcConRead	-> True
       TcConHeadRead	-> True
       TcConDeepRead	-> True
       TcConWrite	-> disjointWrite fargs gcon gargs
       -- A deep write can only work if the rhs is alloc, and we already know it's not
       TcConDeepWrite	-> False
       TcConAlloc	-> True
       TcConDeepAlloc	-> True
       _		-> False

    -- It can't (shouldn't?) be anything else...
    disjoint _ _
     = False

    disjointWrite fargs gcon gargs
     = case gcon of
       -- The only two that might possibly work are writing or reading to a distinct region
       TcConWrite	-> distinct fargs gargs
       TcConRead	-> distinct fargs gargs
       -- Likewise, writing combined with head or deep reads can't work
       _		-> False

    distinct fargs gargs
     | [farg]  <- fargs
     , [garg]  <- gargs
     , Just fb <- boundOf farg
     , Just gb <- boundOf garg
     = areDistinct env fb gb
     | otherwise
     = False

    boundOf (TVar b)
     = Just b
    boundOf (TCon (TyConBound b _))
     = Just b
    boundOf _
     = Nothing

-- | Check if two regions are definitely distinct.
-- We might not know, eg if they're bound in lambdas, so err on false
areDistinct
    :: (Eq n, Ord n, Show n)
    => RE.RewriteEnv a n
    -> Bound n -> Bound n
    -> Bool
areDistinct env p q
 -- Check witness map for "Distinct p q" and vice versa
 | witDistinct
 = True

 -- If they are the same, they can't possibly be different
 | p == q
 = False

 -- If they're both named primitives (eg R0#, R1#)
 -- we can just check name-equality, since can't be bound in lambdas
 -- Or if they're both bound in letregions, we can check by name
 -- (and we know names are different because that's an insta-fail)
 | concrete p && concrete q
 = True

 -- Otherwise... we don't really know.
 | otherwise
 = False
 where
    -- | Check if region is 'concrete' - either global (R0#) or letregion
    concrete r
     = case r of
       UPrim _ _ -> True
       _	 -> RE.containsRegion r env

    witDistinct
      =  RE.containsWitness (wit p q) env
      || RE.containsWitness (wit q p) env

    wit p' q'
      = T.TCon (T.TyConWitness (T.TwConDistinct 2)) `T.TApp` rgn p' `T.TApp` rgn q'

    rgn b
     = case b of
       UPrim _ t -> T.TCon (T.TyConBound b t)
       _	 -> T.TVar b

