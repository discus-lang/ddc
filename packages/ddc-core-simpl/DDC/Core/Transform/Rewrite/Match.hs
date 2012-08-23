-- | Create substitution to make (subst template) == target
module DDC.Core.Transform.Rewrite.Match
    ( match
    , SubstInfo
    , emptySubstInfo )
where

import DDC.Core.Exp
import qualified DDC.Type.Transform.AnonymizeT	as T
import qualified DDC.Core.Transform.AnonymizeX	as T
import qualified DDC.Core.Transform.Reannotate	as T
import qualified DDC.Type.Equiv			as TE
import qualified Data.Map			as Map
import qualified Data.Set			as Set

-- | Substitution: separate values and types
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

-- Variables bound by the rule: restricted to just UName earlier.
match m bs (XVar _ (UName n)) r
 | n `Set.member` bs
 -- Check if it's already been matched
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  -> 
	-- Check if they're equal. Anonymize so names don't matter.
	-- Reannotate so annotations are ignored.
	let  x' = T.anonymizeX $ T.reannotate (const ()) x
	     r' = T.anonymizeX $ T.reannotate (const ()) r
	in if   x' == r'
	   then Just m
	   else Nothing

match m _ (XVar _ l) (XVar _ r)
 | l == r	= Just m

match m _ (XCon _ l) (XCon _ r)
 | l == r	= Just m

match m bs (XApp _ lf la) (XApp _ rf ra)
 = do	m' <- match m bs lf rf
	match m' bs la ra

match m bs (XCast _ lc le) (XCast _ rc re)
 | eqCast lc rc	= match m bs le re

match (xs,tys) bs (XType l) (XType r)
 = do	tys' <- TE.matchT bs tys l r
	return (xs,tys')

match m _ (XWitness l) (XWitness r)
 | eqWit l r	= return m

match _ _ _ _ = Nothing

eqCast lc rc = clean lc == clean rc
 where
  clean c = case c of
    CastWeakenEffect  eff -> CastWeakenEffect  $ T.anonymizeT eff
    CastWeakenClosure clo -> CastWeakenClosure $ map cleanX clo
    CastPurify	      wit -> CastPurify wit
    CastForget	      wit -> CastForget wit

  cleanX = T.anonymizeX . T.reannotate (const ())

eqWit lw rw = lw == rw

