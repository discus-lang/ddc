-- | Create substitution to make (subst template) == target
module DDC.Core.Transform.Rewrite.Match
    ( match
    , SubstInfo
    , emptySubstInfo )
where

import DDC.Core.Exp

import qualified DDC.Type.Equiv as TE

import qualified Data.Map as Map
import qualified Data.Set as Set

type SubstInfo a n = (Map.Map n (Exp a n), Map.Map n (Type n))
emptySubstInfo = (Map.empty, Map.empty)

lookupx n (xs,_) = Map.lookup n xs
insertx n x (xs,tys) = (Map.insert n x xs, tys)


-- | Create substitution to make (subst template) == target
--   Does not handle higher-order templates (ie ones with binders)
-- Eg
--   match emptySubstInfo (Set.fromList [r1, r2, s])
--   (stream [r1]  (unstream [r2]  s))
--   (stream [R0#] (unstream [R1#] (someStream 23))
--  =>
--   { r1 |-> R0#, r2 |-> R1, s |-> someStream 23 }
--
match :: (Show a, Show n, Ord n)
      => SubstInfo a n		-- ^ Current substitution
      -> Set.Set n		-- ^ Variables we're interested in
      -> Exp a n		-- ^ Template
      -> Exp a n		-- ^ Target
      -> Maybe (SubstInfo a n)
match m bs (XVar _ (UName n)) r
 | n `Set.member` bs
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  -> 
	-- Constrain, but with no matching: basically check whether they're equal
	match m Set.empty x r

match m _ (XVar _ l) (XVar _ r)
 | l == r	= Just m

match m _ (XCon _ l) (XCon _ r)
 | l == r	= Just m

match m bs (XApp _ lf la) (XApp _ rf ra)
 = do	m' <- match m bs lf rf
	match m' bs la ra

-- TODO: match the casts
match m bs (XCast _ _lc le) (XCast _ _rc re)
 = match m bs le re

match (xs,tys) bs (XType l) (XType r)
 = do	tys' <- TE.matchT bs tys l r
	return (xs,tys')

match m _ (XWitness l) (XWitness r)
 | eqWit l r	= return m

match _ _ _ _ = Nothing

-- TODO witness equality
eqWit _ _ = True

