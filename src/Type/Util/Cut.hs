-- | Cuts loops in types
--
--   TODO: Remember which fetters we've entered on the way up the tree.
--         Avoid re-entering the same type more than once.
--         This'll probably make it a lot faster when there are a large number of 
--         fetters to inspect.
--
--   For recursive functions, the type we trace from the graph will contain
--   loops in the effect and closure portion of the graph: 
--
--   eg: for the map we have
--
--    *166       :- *166       = *168 -(!180 $181)> *173
--               ,  *168       = *169 -(!171 $172)> *170
--               ,  *173       = *174 -(!178 $179)> *176
--               ,  *174       = Data.List.List %175 *169
--               ,  *176       = Data.List.List %177 *170
--               ,  *757       = forall x %rTS0. x -> Data.List.List %rTS0 x -($cTC29)> Data.List.List %rTS0 x :- $cTC29     = x : x
--    (loop)     ,  !178       :> !{Base.!Read %175; !171; !1770; !180; !178; !1773}
--               ,  $179       :> ${$1759; $1760; $1761; $1762}
--               ,  $181       :> $179 \ f
--               ,  $1759      :> Data.List.Nil : forall %r1 a. Data.List.List %r1 a
--               ,  $1760      :> Data.List.(:) : *757
--               ,  $1761      :> f : *168
--    (loop)     ,  $1762      :> Data.List.map : *166
--
--   We need to break these loops before packing the type into normal form, otherwise
--   the pack process will loop forever. (we can't construct an infinite type)
--
--   For :> constraints on effect and closure classes, we start at the top-most cid
--   and trace through the type, masking classes as we and looking for references to cids
--   which have already been marked. Looping effect and closure classes can be replaced by Bot,
--   and looping data cids create 'cannot construct infinite type' errors.
--
--   It's ok to replace looping effect and closure cids with TBot because $c1 :> $c1 is always
--   trivially satisfied.
--
--	$c1 :> $c2 \/ $c1
--	
--
module Type.Util.Cut
	( cutLoopsT )

where

import Type.Exp
import Type.Plate.Collect
import Type.Pretty
import Type.Error

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

-- Cut loops in this type
cutLoopsT :: Type -> Type
cutLoopsT (TFetters fs cid@(TClass{}))
 = let	
	-- split the fetters into the let/more and the rest.
	(fsLetMore, fsOther)	
			= partition (\f -> f =@= FMore{} || f =@= FLet{}) fs

	-- build a map of let/more fetters so we can look them up easilly.
 	sub		= Map.fromList 
			$ map (\f -> case f of
					FMore t1 t2	-> (t1, f)
					FLet  t1 t2	-> (t1, f))
			$ fsLetMore
	
	-- cut loops in these lets
	fsLetMore'	= cutLoopsF (Set.singleton cid) sub cid
	
	-- rebuild the type, with the new fetters
     in	TFetters (Map.elems fsLetMore' ++ fsOther) cid


cutLoopsF
	:: Set Type
	-> Map Type Fetter
	-> Type
	-> Map Type Fetter

cutLoopsF cidsEntered sub cid
 = case Map.lookup cid sub of

	-- No constructor for this cid
	Nothing	-> sub

	Just fetter
	 -> let
	 	-- update the map so we know we've entered this fetter
		cidsEntered'	= Set.insert cid cidsEntered
 
	 	-- bottom out any back edges in the rhs type and update the map
		fetter'		= cutF (Just cid) cidsEntered' fetter
		sub2		= Map.insert cid fetter' sub
	
	 	-- collect up remaining classIds in this type
		cidsMore	= case fetter' of
					FLet _  t2	-> collectTClasses t2
					FMore _ t2	-> collectTClasses t2
	
		-- decend into branches
		sub3		= foldl' (cutLoopsF cidsEntered') sub2 cidsMore
	
	   in	sub3


-- | Replace TClasses in the type which are members of the set with Bottoms
cutF 	:: Maybe Type -> Set Type -> Fetter -> Fetter
cutF cid cidsEntered ff
 = case ff of
 	FLet  t1 t2	-> FLet  t1 (cutT cid cidsEntered t2)
	FMore t1 t2	-> FMore t1 (cutT cid cidsEntered t2)

cutT cid cidsEntered tt	
 = let down	= cutT cid cidsEntered
   in  case tt of
	TForall  vks tt		-> TForall	 vks (down tt)

	-- These fetters are wrapped around a type in the RHS of our traced type
	--	they're from type which have already been generalised
	TFetters fs  tt		-> TFetters 	(map (cutF_follow cidsEntered) fs) (down tt)

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
		KData		-> panic stage $ "cutT: uncaught loop through class " % cid % "\n"

	 | otherwise
	 -> tt


	TTag{}			-> tt
	TError{}		-> tt
 	
	_ -> panic stage
		$ "cutT: no match for " % tt


-- | Replace TClasses in the fetter which are members of the set with Bottoms
cutF_follow :: Set Type -> Fetter -> Fetter
cutF_follow cidsEntered ff
 = let down	= cutT Nothing cidsEntered
   in case ff of
 	FConstraint 	v ts		-> FConstraint v (map down ts)
	FLet 		t1 t2		-> FLet  t1  (down t2)
	FMore		t1 t2		-> FMore t1  (down t2)
	FProj		j v tDict tBind	-> FProj j v (down tDict) (down tBind)


