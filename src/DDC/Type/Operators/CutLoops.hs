{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Cuts loops though the effect and closure portions of types.
--
--   For recursive functions, the type we trace from the graph will contain
--   loops though the effect and closure portion of the graph. For example, 
--   for @map@ we have:
--
-- @
--    *166       :- *166   = *168 -(!180 $181)> *173
--               ,  *168   = *169 -(!171 $172)> *170
--               ,  *173   = *174 -(!178 $179)> *176
--               ,  *174   = Data.List.List %175 *169
--               ,  *176   = Data.List.List %177 *170
--               ,  *757   = forall x %rTS0. x -> Data.List.List %rTS0 x -($cTC29)> Data.List.List %rTS0 x 
--						:- $cTC29     = x : x
--    (loop)     ,  !178   :> !{Base.!Read %175; !171; !1770; !180; !178; !1773}
--               ,  $179   :> ${$1759; $1760; $1761; $1762}
--               ,  $181   :> $179 \ f
--               ,  $1759  :> Data.List.Nil : forall %r1 a. Data.List.List %r1 a
--               ,  $1760  :> Data.List.(:) : *757
--               ,  $1761  :> f : *168
--    (loop)     ,  $1762  :> Data.List.map : *166
-- @
--
--   We need to break these loops before packing the type into normal form, otherwise
--   the pack process will loop forever (can't construct an infinite type).
--
--   For @:>@ constraints on effect and closure types, we start at the top-most cid
--   and trace through the type, masking classes as we go and looking for references to cids
--   which have already been marked. If we find a cid we have already entered then we replace
--   it by bottom. This is valid because a looping more-than constraint is equivalent to 
--   $t1 :> t1$, which is always satisfied. 
--
-- 	
--  TODO: This is at least O(n^2) work. 
--        We should be smarter about computing the reachability graph.
--
module DDC.Type.Operators.CutLoops
	(cutLoopsT_constrainForm)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.FreeTVars
import DDC.Type.Pretty		()
import Data.List
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Data.Set			(Set)

stage	= "DDC.Type.CutLoops"

-- | Cut loops through the effect and closure portions of this type.
--	The type should have an outer `TConstrain`, else this function is the identity.
cutLoopsT_constrainForm :: Type -> Type
cutLoopsT_constrainForm (TConstrain tt crs)
 = let	-- decend into the constraints from every cid in the body of the type
	cidsRoot	= Set.toList $ freeTClasses tt
	crs'		= foldl' (cutLoops1 Set.empty) crs cidsRoot
   in	TConstrain tt crs'

cutLoopsT_constrainForm tt
 	= tt

-- | Cut loops in the constraint with this cid
cutLoops1 
	:: Set Type		-- ^ Set of cids we've entered so far
	-> Constraints		-- ^ The current constraints
	-> Type			-- ^ Cid of the constraint we're decending into
	-> Constraints		-- ^ The new constraints
	
cutLoops1 cidsEntered crs@(Constraints crsEq crsMore crsOther) tCid
	| Just t2	<- Map.lookup tCid crsEq
	= cutLoops1' cidsEntered tCid crsEq   (\crsX -> Constraints crsX crsMore crsOther) t2

	| Just t2	<- Map.lookup tCid crsMore
	= cutLoops1' cidsEntered tCid crsMore (\crsX -> Constraints crsEq crsX crsOther) t2

	| otherwise
	= crs

	
cutLoops1' cidsEntered tCid crsX updateCrsX t2
 = let
	-- remember that we've entered this constraint
	cidsEntered'	= Set.insert tCid cidsEntered
		
	-- cut cids in this constraint
	t2'		= cutT cidsEntered' t2
	crsX'		= Map.insert tCid t2' crsX
	crs'		= updateCrsX crsX'
		
	-- decend into other constraints reachable from this type
	cidsMore	= Set.toList $ freeTClasses t2'
	 
   in	foldl' (cutLoops1 cidsEntered') crs' cidsMore
	

cutT cidsCut tt	
 = let down	= cutT cidsCut
   in  case tt of
	TForall  b k t		-> TForall b k (down t)

	TConstrain t (Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.map (cutT cidsCut) crsEq
		crsMore'	= Map.map (cutT cidsCut) crsMore
		crsOther'	= map (cutF cidsCut) crsOther
	    in	TConstrain (down t) (Constraints crsEq' crsMore' crsOther')
	
	TSum  k ts		-> TSum k (map down ts)
	TCon{}			-> tt

	TApp{}
	 | Just (_, t2@(TVar _ (UClass _)))	<- takeTFree tt
	 ,  Set.member t2 cidsCut
	 -> tEmpty

	TApp	t1 t2		-> TApp (down t1) (down t2)

	TVar k (UClass cid')
	 | Set.member tt cidsCut
	 -> let result
	 	 | k == kEffect	 = tPure
		 | k == kClosure = tEmpty

		 | otherwise
		 = panic stage $ "cutT: uncaught loop through class " % cid' % "\n"
	    in  result

	 | otherwise
	 -> tt

	TVar{}			-> tt

	TError{}		-> tt
 	
	_ -> panic stage
		$ "cutT: no match for " % tt % "\n"
		% show tt


-- | Replace TClasses in the fetter which are members of the set with Bottoms
cutF :: Set Type -> Fetter -> Fetter
cutF cidsEntered ff
 = let down	= cutT cidsEntered
   in case ff of
 	FConstraint v ts	-> FConstraint v (map down ts)
	FWhere t1 t2		-> FWhere t1  (down t2)
	FMore  t1 t2		-> FMore  t1  (down t2)
	FProj  j v tDict tBind	-> FProj  j v (down tDict) (down tBind)

