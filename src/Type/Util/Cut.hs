-- | Cuts loops in types
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
--               ,  *757       = forall x %rTS0. x -> Data.List.List %rTS0 x -($cTC29)> Data.List.List %rTS0 x 
--						:- $cTC29     = x : x
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
--  TODO: Remember which fetters we've entered on the way up the tree.
--        Avoid re-entering the same type more than once.
--        This'll probably make it a lot faster when there are a large number of 
--        fetters to inspect.
--
module Type.Util.Cut
	(cutLoopsT_constrainForm)
where
import Type.Plate.Collect
import Type.Exp
import Type.Builtin
import Util
import DDC.Main.Error
import Type.Pretty		()
import qualified Data.Map	as Map
import qualified Data.Set	as Set

-----
stage	= "Type.Util.Cut"


-- | Cut loops in this type
cutLoopsT_constrainForm :: Type -> Type
cutLoopsT_constrainForm (TConstrain tt crs)
 = let	-- decend into the constraints from every cid in the body of the type
	cidsRoot	= Set.toList $ collectTClasses tt
	crs'		= foldl' (cutLoops1 Set.empty) crs cidsRoot
   in	TConstrain tt crs'

cutLoopsT_constrainForm tt
 	= tt


-- | Cut loops in the constraint with this cid
cutLoops1 
	:: Set Type		-- ^ the set of cids we've entered so far
	-> Constraints		-- ^ the current constraints
	-> Type			-- ^ the cid of the constraint we're decending into
	-> Constraints		-- ^ the new constraints
	
cutLoops1 cidsEntered crs@(Constraints crsEq crsMore crsOther) tCid
	| Just t2	<- Map.lookup tCid crsEq
	= cutLoops1' cidsEntered crs tCid crsEq   (\crsX -> Constraints crsX crsMore crsOther) t2

	| Just t2	<- Map.lookup tCid crsMore
	= cutLoops1' cidsEntered crs tCid crsMore (\crsX -> Constraints crsEq crsX crsOther) t2

	| otherwise
	= crs

	
cutLoops1' cidsEntered crs tCid crsX updateCrsX t2
 = let
	-- remember that we've entered this constraint
	cidsEntered'	= Set.insert tCid cidsEntered
		
	-- cut cids in this constraint
	t2'		= cutT cidsEntered' t2
	crsX'		= Map.insert tCid t2' crsX
	crs'		= updateCrsX crsX'
		
	-- decend into other constraints reachable from this type
	cidsMore	= Set.toList $ collectTClasses t2'
	 
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
	TVar{}			-> tt
	TCon{}			-> tt

	TApp	t1 t2		-> TApp (down t1) (down t2)
	TEffect	v ts		-> TEffect v (map down ts)

	TFree v t2@(TClass k _)
	 |  Set.member t2 cidsCut
	 -> tEmpty

	TFree v t		-> TFree   v (down t)
	TDanger t1 t2		-> TDanger (down t1) (down t2)

	TClass k cid'
	 | Set.member tt cidsCut
	 -> let result
	 	 | k == kEffect	 = tPure
		 | k == kClosure = tEmpty
		 | k == kValue	 
		 = panic stage $ "cutT: uncaught loop through class " % cid' % "\n"
	    in  result

	 | otherwise
	 -> tt

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

