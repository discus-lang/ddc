{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Trimming of closures.
--
--   Inferred closure terms tend to contain a lot of information that isn't useful to the inferencer,
--   or core language transforms. We trim most of it out to save time in later stages, and make the 
--   code easier to read.
--
--   For example, in a function type like @a -(%e1 $c1)> b@ only the @$c1@ part can manifest
--   region variables that we have to worry about during generalisation etc.
--
--   NOTE: when trimming under foralls, we must retain the quantification of manifest variables.
--         For example, the following:
--
--   @
--	forall a %r1. a -($c1)> b
--	:- $c1 = Thing a %r1 %r2
--   @
--
--   is trimmed to:
--
--   @	forall a %r1. Thing a %r1 %r2@
--
--   The variable @%r1@ was quantified in the original, so must also be quantified in the result.
--
module DDC.Type.Operators.Trim
	( trimClosureT
	, trimClosureC)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Type.Operators.Pack
import DDC.Var
import DDC.Util.FreeVars
import DDC.Type.FreeVars		()
import DDC.Type.Pretty			()
import Data.Maybe
import Data.Set				(Set)
import qualified Shared.VarUtil		as Var
import qualified Data.Set		as Set
import qualified Data.Map		as Map
import qualified Debug.Trace		as Debug

stage		= "DDC.Type.Trim"
debug		= False
trace ss x	= if debug then Debug.trace (pprStrPlain ss) x else x


-- Type -------------------------------------------------------------------------------------------
-- | Trim the closure portion of this type.
trimClosureT :: Type -> Type
trimClosureT tt
	= trimClosureT' Set.empty Set.empty tt

trimClosureT' quant rsData tt
  = trace ("trimClosureT " % tt % "\n")
  $ let	tt_trimmed	= trimClosureT_trace quant rsData tt
	tt_packFast	= packType tt_trimmed
			
	tt'		= trace ( "tt_trimmed  = " % tt_trimmed 	% "\n"
				% "tt_packFast = " % tt_packFast	% "\n\n")
		 		$ tt_packFast
		
    in	if tt' == tt
    		then tt'
		else trimClosureT' quant rsData tt'

trimClosureT_trace quant rsData tt		
 = let down	= trimClosureT' quant rsData
   in  case tt of
	TVar{}		-> tt
	TCon{}		-> tt

	TSum k _
	 | isClosureKind k	-> trimClosureC tt
	 | otherwise		-> tt

	TApp t1 t2
	 | Just _	<- takeTFree tt
	 -> trimClosureC' quant rsData tt

	 | otherwise	-> TApp (down t1) (down t2)

	TForall b k t
	 -> TForall b k (down t)
	
	TConstrain tBody (Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.mapWithKey (trimClosureT_tt quant rsData) crsEq
		crsMore'	= Map.mapWithKey (trimClosureT_tt quant rsData) crsMore
		crs'		= Constraints crsEq' crsMore' crsOther
	    in	addConstraints crs' tBody

	_ -> panic stage
		$ "trimClosureT: no match for " % show tt


-- Closure ----------------------------------------------------------------------------------------
-- | Trim a closure down to its interesting parts.
trimClosureC :: Closure -> Closure
trimClosureC cc
	= trimClosureC' Set.empty Set.empty cc

trimClosureC' quant rsData cc
 = let 	cc_trimmed	= trimClosureC_trace quant rsData cc
	cc_packed	= packType $ cc_trimmed
		
	cc'		= trace 
 				( "trimClosureC\n"	
 				% "    rsData   = " % rsData		% "\n"
				% "    closure  = " % cc		% "\n"
				% "    trimmed  = " % cc_trimmed	% "\n")
				cc_packed
   in	if cc' == cc
   	 then cc'
	 else trimClosureC' quant rsData cc'
	
trimClosureC_trace quant rsData cc
 = let down	= trimClosureC' quant rsData
   in  case cc of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar{}
		| Set.member cc quant 
		-> tEmpty

		| otherwise		
		-> makeTSum kClosure
		$ (cc : [makeTDanger r cc	
				| r	<- Set.toList rsData
				, r /= cc])
	
	-- Trim all the elements of a sum
	TSum{}
		-> makeTSum kClosure 
		$  map down
		$  flattenTSum cc

	TConstrain t Constraints { crsEq, crsMore }
	 -> let	t'		= trimClosureC' quant rsData t
		crsEq'		= Map.mapWithKey (\_ t2 -> trimClosureT' quant rsData t2) crsEq
		crsMore'	= Map.mapWithKey (\_ t2 -> trimClosureT' quant rsData t2) crsMore
	    in	addConstraints (Constraints crsEq' crsMore' []) t'

	-- add quantified vars to the set
	TForall b k t		
	 -> let Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k (UVar v)) quant
	    in	trimClosureC' quant' rsData t

	-- free
	TApp{}
	 | Just (tag, t1)	<- takeTFree   cc
	 , Just (t11, t12)	<- takeTDanger t1
	 , isRegion t11
	 , isRegion t12
	 -> makeTSum kClosure 
		[ makeTFreeBot tag t11
		, makeTFreeBot tag t12 ]
	
	 -- Free tag (TDanger t11 (TDanger t121 t122)) 
	 --	=> ${tag : t11 $> t121;  tag : t11 $: t122;  tag : t121 $> t122}
	 | Just (tag, t1)	<- takeTFree cc
	 , Just (t11, t12)	<- takeTDanger t1
	 , Just (t121, t122)	<- takeTDanger t12
	 -> makeTSum kClosure
	 	[ makeTFreeBot tag (makeTDanger t11  t121)
		, makeTFreeBot tag (makeTDanger t11  t122)
		, makeTFreeBot tag (makeTDanger t121 t122) ]

	 | Just (tag, t1)	<- takeTFree cc
	 , Just (t11, t12)	<- takeTDanger t1
	 -> if isClosure t12
		then makeTFreeBot tag 
			  $ makeTDangerIfRegion tag t11 (down t12)

		else makeTSum kClosure
			  $ map (makeTFreeBot tag)
			  $ map (makeTDangerIfRegion tag t11)
			  $ trimClosureC_t tag quant rsData t12

	 | Just (_, t)		<- takeTFree cc
	 , isEffect t		
	 -> tEmpty
	
	 | Just (tag, t)	<- takeTFree cc
	 -> if isClosure t
		then makeTFreeBot tag $ down t
		else makeTFreeBot tag $ makeTSum kClosure 
				   $ trimClosureC_t tag quant rsData t

	 | Just _		<- takeTDanger cc
	 -> cc
	
	_ -> panic stage
		$ "trimClosureC: no match for " % cc



-- | Trim a value type element of a closure.
trimClosureC_t :: Var -> Set Type -> Set Type -> Type -> [Type]
trimClosureC_t tag quant rsData tt
 = let down	= trimClosureC_t tag quant rsData
   in  case tt of
	-- if some var has been quantified by a forall then it's not free
	--	and not part of the closure
	TVar{}
		| Set.member tt quant	-> []
		| otherwise		-> makeFreeDanger tag rsData tt

	-- Trim the fetters of this data
	TConstrain tBody (Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsEq
		crsMore'	= Map.fromList $ mapMaybe (trimClosureC_tt quant rsData) $ Map.toList crsMore
		crs		= Constraints crsEq' crsMore' crsOther
	    	cBits		= down tBody
	    in	map (addConstraints crs) cBits
			
	-- Trim under foralls
	TForall BNil _ t
	 -> trimClosureC_t tag quant rsData t

	TForall b k t		
	 -> let	Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k (UVar v)) quant
	    in	trimClosureC_t tag quant' rsData t
	
	TSum _ ts	-> concatMap down ts

	TCon{}		-> []

	-- when we enter into a data object remember that we're under its primary region.
	TApp{}
	 | Just (_, _, [])	<- takeTData tt 
	 -> []
		
	 | Just (_, _, (t:ts))	<- takeTData tt
	 -> 
	    -- If the primary region is mutable then all the variables under it are dangerous.
	    if isRegion t
	     then let 	rsData'	= Set.insert t rsData
			vs	= freeVars (t:ts)
	    	   in  	concatMap (trimClosureC_t tag quant rsData') (t:ts)
			  ++ map (makeTDanger t) 
				[TVar k (UVar v)
					| v <- Set.toList vs 
					, not $ Var.isCtorName v
					, let Just k = kindOfSpace $ varNameSpace v]
	     else concatMap down ts

	 | Just (_, _, _, clo) <- takeTFun tt
	 -> down clo

	 | Just (_, _)		<- takeTFree tt
	 -> [trimClosureC' quant rsData tt]
		
	 | otherwise
	 -> []
	
	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- Fetter -----------------------------------------------------------------------------------------
-- | Trim a fetter of a closure
trimClosureC_tt 
	:: Set Type 
	-> Set Type 
	-> (Type, Type)
	-> Maybe (Type, Type)

trimClosureC_tt quant rsData (c1, c2)
 	| isClosure c1
	= Just (c1, trimClosureC' quant rsData c2)
	
	| isEffect c1
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
	| isClosure c1
	= trimClosureC' quant rsData c2
	
	| otherwise
	= c2



makeFreeDanger tag rsData t
	| Set.null rsData	= [t]

	| otherwise		
	= map (\r -> makeTDangerIfRegion tag r t) 
	$ Set.toList rsData

makeTDangerIfRegion tag r t
	| isRegion t	= makeTFreeBot tag t
	| otherwise	= makeTDanger r t
