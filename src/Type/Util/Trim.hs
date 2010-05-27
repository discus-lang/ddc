
-- | Trimming of closures
--	Inferred closure tend to contain a lot of information that isn't useful to the solver
--	or core IR. We can trim out a lot of this superfulous stuff.
--
--	We're only interested in data contructors.
--
--	We only need the closure part of functions:
--		ie   a -(%e1 $c1)> b
--		only the $c1 part can contain data.
--
--	Note: trimming under foralls, must retain quantification of some vars.
--
--		forall a %r1. a -($c1)> b
--		:- $c1 = Thing a %r1 %r2
--
--	reduce to
--		forall a %r1. Thing a %r1 %r2	

module Type.Util.Trim
	( trimClosureT_constrainForm
	, trimClosureC_constrainForm)
where
import Util
import Type.Exp
import Type.Builtin
import Type.Util.Bits
import Type.Util.Kind
import Type.Plate.FreeVars
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Var
import Type.Pretty			()
import qualified Type.Util.PackFast	as PackFast
import qualified Shared.VarUtil		as Var
import qualified Data.Set		as Set
import qualified Data.Map		as Map
import qualified Debug.Trace		as Debug

stage		= "Type.Util.Trim"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain ss) x else x


-- | Trim the closure portion of this type
trimClosureT_constrainForm
	:: Set Type	-- ^ variables that are quantified in this context
	-> Set Type	-- ^ primary region variables of this context
	-> Type 
	-> Type

trimClosureT_constrainForm quant rsData tt
	= trimClosureT_start quant rsData tt

trimClosureT_start quant rsData tt
  = trace ("trimClosureT " % tt % "\n")
  $ let	tt_trimmed	= trimClosureT' quant rsData tt
	tt_packFast	= PackFast.packType tt_trimmed
			
	tt'		= trace ( "tt_trimmed  = " % tt_trimmed 	% "\n"
				% "tt_packFast = " % tt_packFast	% "\n\n")
		 		$ tt_packFast
		
    in	if tt' == tt
    		then tt'
		else trimClosureT_start quant rsData tt'

trimClosureT' quant rsData tt		
 = case tt of
	TConstrain tBody crs@(Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.mapWithKey (trimClosureT_tt quant rsData) crsEq
		crsMore'	= Map.mapWithKey (trimClosureT_tt quant rsData) crsMore
		crs'		= Constraints crsEq' crsMore' crsOther
	    in	addConstraints crs' tBody

	TApp{}
	 | Just _	<- takeTFree tt
	 -> trimClosureC_constrainForm quant rsData tt

	_	-> tt


-- | Trim a closure down to its interesting parts
trimClosureC_constrainForm :: Set Type -> Set Type -> Closure -> Closure
trimClosureC_constrainForm quant rsData cc
	= trimClosureC_start quant rsData cc

trimClosureC_start quant rsData cc
 = let 	cc_trimmed	= trimClosureC' quant rsData cc
	cc_packed	= PackFast.packType $ cc_trimmed
			
	cc'		= trace 
 				( "trimClosureC\n"	
 				% "    rsData   = " % rsData		% "\n"
				% "    closure  = " % cc		% "\n"
				% "    trimmed  = " % cc_trimmed	% "\n")
				cc_packed
   in	if cc' == cc
   	 then cc'
	 else trimClosureC_start quant rsData cc'
	
trimClosureC' quant rsData cc
 = let down	= trimClosureC_start quant rsData
   in  case cc of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar k v
		| Set.member cc quant 	
		-> tEmpty

		| otherwise		
		-> makeTSum kClosure
			$ (cc : [makeTDanger r cc	
					| r	<- Set.toList rsData
					, r /= cc])

	-- cids are never quantified so we always have to keep them.
	TClass k _	
		-> makeTSum kClosure
			$ cc : [makeTDanger r cc	
					| r	<- Set.toList rsData
					, r /= cc]

	-- Trim all the elements of a sum
	TSum k cs	
		-> makeTSum kClosure 
		$  map down
		$  flattenTSum cc

	TConstrain t crs@Constraints { crsEq, crsMore, crsOther }
	 -> let	t'		= trimClosureC_start quant rsData t
		crsEq'		= Map.mapWithKey (\t1 t2 -> trimClosureT_constrainForm quant rsData t2) crsEq
		crsMore'	= Map.mapWithKey (\t1 t2 -> trimClosureT_constrainForm quant rsData t2) crsMore
	    in	addConstraints (Constraints crsEq' crsMore' []) t'

	-- add quantified vars to the set
	TForall b k t		
	 -> let Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k v) quant
	    in	trimClosureC_start quant' rsData t

	-- free
	TApp{}
	 | Just (tag, t1)	<- takeTFree   cc
	 , Just (t11, t12)	<- takeTDanger t1
	 , isRegion t11
	 , isRegion t12
	 -> makeTSum kClosure 
		[ makeTFree tag t11
		, makeTFree tag t12 ]
	
	 -- Free tag (TDanger t11 (TDanger t121 t122)) 
	 --	=> ${tag : t11 $> t121;  tag : t11 $: t122;  tag : t121 $> t122}
	 | Just (tag, t1)	<- takeTFree cc
	 , Just (t11, t12)	<- takeTDanger t1
	 , Just (t121, t122)	<- takeTDanger t12
	 -> makeTSum kClosure
	 	[ makeTFree tag (makeTDanger t11  t121)
		, makeTFree tag (makeTDanger t11  t122)
		, makeTFree tag (makeTDanger t121 t122) ]

	 | Just (tag, t1)	<- takeTFree cc
	 , Just (t11, t12)	<- takeTDanger t1
	 -> if isClosure t12
		then makeTFree tag 
			  $ makeTDangerIfRegion tag t11 (down t12)
		else makeTSum kClosure
			  $ map (makeTFree tag)
			  $ map (makeTDangerIfRegion tag t11)
			  $ trimClosureC_t tag quant rsData t12

	 | Just (tag, t)	<- takeTFree cc
	 -> if isClosure t
		then makeTFree tag $ down t
		else makeTFree tag $ makeTSum kClosure 
				   $ trimClosureC_t tag quant rsData t

	 | Just _		<- takeTDanger cc
	 -> cc
	
	_ -> panic stage
		$ "trimClosureC: no match for " % show cc



-- | Trim a value type element of a closure.
trimClosureC_t :: Var -> Set Type -> Set Type -> Type -> [Type]
trimClosureC_t tag quant rsData tt
 = let tsBits	= trimClosureC_t' tag quant rsData tt
   in  {- trace 	( "trimClosureC_t " % tt % "\n"
   		% "    rsData: " %> rsData	% "\n"
   		% "    tsBits: " %> tsBits	% "\n")	$  -}
		tsBits
 
trimClosureC_t' tag quant rsData tt
 = let down	= trimClosureC_t tag quant rsData
   in  case tt of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar k v
		| Set.member tt quant	-> []
		
		| otherwise		
		-> makeFreeDanger tag rsData tt

	-- classids are never quantified, so we always have to keep them.
	TClass{} 	
		-> makeFreeDanger tag rsData tt

	-- Trim the fetters of this data
	TConstrain tBody crs@(Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsEq
		crsMore'	= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsMore
		crs		= Constraints crsEq' crsMore' crsOther
	    	cBits		= down tBody
	    in	map (addConstraints crs) cBits
			
	-- Trim under foralls
	TForall b k t		
	 -> let	Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k v) quant
	    in	trimClosureC_t tag quant' rsData t
	
	TSum k ts	-> catMap down ts

	TCon{}		-> []

	-- when we enter into a data object remember that we're under its primary region.
	TApp{}
	 | Just (v, k, [])	<- takeTData tt 
	 -> []
		
	 | Just (v, k, (t:ts))	<- takeTData tt
	 -> if kindOfType_orDie t == kRegion 
	     then let 	rsData'	= Set.insert t rsData
			vs	= freeVars (t:ts)
	    	   in  	catMap (trimClosureC_t tag quant rsData') (t:ts)
			  ++ map (makeTDanger t) 
				[TVar (kindOfSpace $ varNameSpace v) v
					| v <- Set.toList vs 
					, not $ Var.isCtorName v]
	     else catMap down ts

	 | Just (t1, t2, eff, clo) <- takeTFun tt
	 -> down clo

	 | Just (v, t)	<- takeTFree tt
	 -> [trimClosureC_start quant rsData tt]
		
	 | otherwise
	 -> []
	
	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"

makeFreeDanger tag rsData t
	| Set.null rsData	= [t]

	| otherwise		
	= map (\r -> makeTDangerIfRegion tag r t) 
	$ Set.toList rsData

makeTDangerIfRegion tag r t
	| kindOfType_orDie t == kRegion
	= makeTFree tag t
	
	| otherwise	
	= makeTDanger r t


-- | Trim a fetter of a closure
trimClosureC_tt 
	:: Set Type 
	-> Set Type 
	-> (Type, Type)
	-> Maybe (Type, Type)

trimClosureC_tt quant rsData (c1, c2)
 	| kindOfType_orDie c1 == kClosure
	= Just (c1, trimClosureC_start quant rsData c2)
	
	| kindOfType_orDie c1 == kEffect
	= Just (c1, c2)
	
	| otherwise
	= Nothing
	

-- | Trim the closure in this fetter.
--	where the fetter was on a type.
trimClosureT_tt 
	:: Set Type
	-> Set Type
	-> Type -> Type
	-> Type

trimClosureT_tt quant rsData c1 c2
	| kindOfType_orDie c1 == kClosure
	= trimClosureC_start quant rsData c2
	
	| otherwise
	= c2
