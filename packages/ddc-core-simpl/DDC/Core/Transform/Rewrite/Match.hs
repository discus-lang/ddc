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


match :: (Show a, Show n, Ord n)
      => SubstInfo a n		-- ^ current substitution
      -> Set.Set n			-- ^ variables we're interested in
      -> Exp a n			-- ^ template
      -> Exp a n			-- ^ target
      -> Maybe (SubstInfo a n)
{-
match _m _bs l r
 | trace ("L:"++show l++"\nR:"++show r) False
 = Nothing
 -}
match m bs (XVar _ (UName n)) r
 | n `Set.member` bs
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  -> 
	-- constrain, but with no matching: basically check whether they're equal
	match m Set.empty x r

{-
match m bs (XType (TVar (UName n _))) r
 | n `Set.member` bs
 = case lookupx n m of
   Nothing -> return $ insertx n r m
   Just x  -> match m [] x r
-}

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

{-
 don't allow binders in lhs of rule, so don't need to worry about these...

-- we could anonymise these bindings. probably should
match m bs (XLAM _ l lb) (XLAM _ r rb)
 | l == r	= match m bs lb rb

match m bs (XLam _ l lb) (XLam _ r rb)
 | l == r	= match m bs lb rb

match m bs (XLet _ ll le) (XLet _ rl re)
 = do	m' <- matchLets m ll rl
	match m' le re

matchLets m (LLet _ lb le) (LLet _ rb re)
 | l == r	= match m le re
matchLets m (LRec ls) (LRec rs)
 | all $ zipWith (==) (map fst ls) (map fst rs)
		= map 
-}

-- TODO witness equality
eqWit _ _ = True

