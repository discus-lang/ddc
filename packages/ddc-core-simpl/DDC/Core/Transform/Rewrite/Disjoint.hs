module DDC.Core.Transform.Rewrite.Disjoint
    ( checkDisjoint )
where

import DDC.Core.Exp

import qualified DDC.Core.Transform.Rewrite.Env	as RE

import qualified DDC.Type.Compounds		as T
--import qualified DDC.Type.Exp			as T
import qualified DDC.Type.Sum			as TS
import qualified DDC.Type.Transform.Crush	as TC

checkDisjoint
    :: (Eq n, Ord n, Show n)
    => Type n		-- ^ target, might be "Disjoint f g"
    -> RE.RewriteEnv n	-- ^ for distinctness map
    -> Bool
checkDisjoint c env
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

    -- *anything goes* with allocations
    disjoint _ (TCon (TyConSpec TcConAlloc) : _)
     = True
    disjoint _ (TCon (TyConSpec TcConDeepAlloc) : _)
     = True

    -- check a variable. since we have no idea what it may end up being,
    -- assume the worst. it can only work if the right-hand is an Alloc
    -- TODO: user-defined effects?
    disjoint (TVar _ : _) _
     = False

    -- check a builtin effect. if it's a read or alloc, assume rhs is OK
    -- (other direction will be checked later).
    -- if it's a write, check which regions and if it overlaps with rhs.
    disjoint (TCon (TyConSpec fcon) : fargs) (TCon (TyConSpec gcon) : gargs)
     = case fcon of
         TcConRead	-> True
         TcConHeadRead	-> True
         TcConDeepRead	-> True
         TcConWrite	-> disjointWrite fargs gcon gargs
       -- a deep write can only work if the rhs is alloc, and we already know it's not
         TcConDeepWrite	-> False
         TcConAlloc	-> True
         TcConDeepAlloc	-> True
	 _		-> False

    -- it can't (shouldn't?) be anything else...
    disjoint _ _
     = False

    disjointWrite fargs gcon gargs
     = case gcon of
       -- the only two that might possibly work are writing or reading to a distinct region
         TcConWrite	-> distinct fargs gargs
         TcConRead	-> distinct fargs gargs
       -- likewise, writing combined with head or deep reads can't work
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
    boundOf (TCon (TyConBound b))
     = Just b
    boundOf _
     = Nothing

-- | Check if two regions are definitely distinct.
-- We might not know, eg if they're bound in lambdas, so err on false
areDistinct
    :: (Eq n, Ord n, Show n)
    => RE.RewriteEnv n
    -> Bound n -> Bound n
    -> Bool
areDistinct env p q
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
 -- TODO Check witness map for "Distinct p q" then give up
 | otherwise
 = False
 where
    -- | Check if region is 'concrete' - either global (R0#) or letregion
    concrete (UPrim _ _) = True
    concrete r = RE.containsRegion r env

