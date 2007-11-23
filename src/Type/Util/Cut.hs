
-- | Cuts loops in types
--	TODO: 	remember which fetters we've entered on the way up the tree.
--		Avoid re-entering the same type more than once.


module Type.Util.Cut
	( cutLoopsT )

where

import Type.Exp
import Type.Plate.Collect
import Type.Pretty

import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)
import qualified Data.Set	as Set
import Data.Set			(Set)
import Data.List

import Debug.Trace

-----
stage	= "Type.Util.Cut"


cutLoopsT :: Type -> Type
cutLoopsT (TFetters fs cid@(TClass{}))
 = let	
	-- split the fetters into the lets and the rest
	(lets, others)	= splitFs [] [] fs

	-- build a map of lets so we can look them up easilly.
 	sub		= Map.fromList 
			$ [(t1, t2) 	| FLet t1 t2	<- lets ]
	
	-- cut loops in these lets
	lets'		= cutLoopsF (Set.singleton cid) sub cid
	
	-- rebuild the type, with the new fetters
     in	TFetters 
     		([FLet t1 t2	| (t1, t2)	<- Map.toList lets']
		++ others)
		cid


cutLoopsF
	:: Set Type
	-> Map Type Type
	-> Type
	-> Map Type Type

cutLoopsF cidsEntered sub cid
 = case Map.lookup cid sub of

	-- No constructor for this cid
	Nothing	-> sub

	Just tt	
	 -> let
	 	-- update the map so we know we've entered this fetter
		cidsEntered'	= Set.insert cid cidsEntered
 
	 	-- bottom out any back edges in the rhs type and update the map
		tt'		= cutT (Just cid) cidsEntered' tt
		sub2		= Map.insert cid tt' sub
	
	 	-- collect up remaining classIds in this type
		cidsMore	= collectTClasses tt'
	
		-- decend into branches
		sub3		= foldl' (cutLoopsF cidsEntered') sub2 cidsMore
	
	   in	sub3


-- | Replace TClasses in the type which are members of the set with Bottoms
cutT 	:: Maybe Type -> Set Type -> Type -> Type
cutT = cutT'
{-
 = case mCid of
 	(Just (TClass k (ClassId i)))
	 | i == 266
	 -> let tt'	= cutT' mCid cidsEntered tt
	    in trace (pretty 
 		$ "cutT: "
		% "   cids = " % Set.toList cidsEntered	% "\n"
		% "   tt   = " % tt			% "\n"
		% "   tt'  = " % tt'			% "\n")
		$ tt'
		
	_	-> cutT' mCid cidsEntered tt
-}

cutT' cid cidsEntered tt	
 = let down	= cutT cid cidsEntered
   in  case tt of
	TForall  vks tt		-> TForall	 vks (down tt)

	-- These fetters are wrapped around a type in the RHS of our traced type
	--	they're from type which have already been generalised
	TFetters fs  tt		-> TFetters 	(map (cutF cidsEntered) fs) (down tt)

	TSum  k ts		-> TSum 	k (map down ts)
	TMask k t1 t2		-> TMask 	k (down t1) (down t2)
	TVar{}			-> tt
	TTop{}			-> tt
	TBot{}			-> tt

	TData	v ts		-> TData 	v (map down ts)
	TFun 	t1 t2 eff clo	-> TFun 	(down t1) (down t2) (down eff) (down clo)

	TEffect	v ts		-> TEffect	v (map down ts)

	TFree v t2@(TClass k cid)
	 |  Set.member t2 cidsEntered
	 -> TBot KClosure

	TFree v t		-> TFree	v (down t)

	TClass k cid
	 |  Set.member tt cidsEntered
	 -> case k of
	 	KEffect		-> TBot KEffect
		KClosure	-> TBot KClosure

	 | otherwise
	 -> tt


	TTag{}			-> tt

		
 	
	_ -> panic stage
		$ "cutT: no match for " % show tt


-- | Replace TClasses in the fetter which are members of the set with Bottoms
cutF 	:: Set Type -> Fetter -> Fetter
cutF cidsEntered ff
 = let down	= cutT Nothing cidsEntered
   in case ff of
 	FConstraint 	v ts	-> FConstraint v (map down ts)
	FLet 		t1 t2	-> FLet t1 (down t2)



-- | Split a list of fetters into FLet and others
splitFs :: [Fetter] -> [Fetter] -> [Fetter] -> ([Fetter], [Fetter])
splitFs lets others []
	= (lets, others)
	
splitFs lets others (x:xs)
 = case x of
 	FLet{}		-> splitFs (x : lets) others xs
	_		-> splitFs lets (x : others) xs
	
	
