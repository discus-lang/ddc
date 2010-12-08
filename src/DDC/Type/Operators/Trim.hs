{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Trimming of closures.
--
--   Inferred closure terms tend to contain a lot of information that isn't useful to the inferencer,
--   or core language transforms. We trim most of it out to save time in later stages, and make the 
--   code easier to read.
--
--   For example, in a function type like @a -(%e1 $c1)> b@ only the @$c1@ part can contain
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
--   TODO: This module is really buggy, and parts of it may be outright wrong.
--         When I started it I didn't know how it was supposed to work.
--         It needs to be rewritten to use the materiality information.
-- 
-- 
module DDC.Type.Operators.Trim
	( trimClosureT
	, trimClosureNoDangerT

	, trimClosureC
	, trimClosureNoDangerC)
where
import DDC.Main.Pretty
import DDC.Main.Error
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Predicates
import DDC.Type.Compounds
import DDC.Type.Kind
import DDC.Var
import DDC.Util.FreeVars
import DDC.Type.Operators.Pack
import DDC.Type.Collect.FreeVars	()
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


-- Config -----------------------------------------------------------------------------------------
data Config
	= Config { 
	
	-- | Return danger (%r1 $> v) terms in the closure.
	--   We want these during type inference to control generalisation, but they're 
	--   not needed in the core program.
	  configMakeDanger	:: Bool }
	

-- Type -------------------------------------------------------------------------------------------
-- | Trim the closure portion of this type.
trimClosureT :: Type -> Type
trimClosureT tt
 = let	config	= Config { configMakeDanger	= True }
   in	trimClosureT' config Set.empty Set.empty tt

-- | Trim the closure portion of this type, without producing TDanger terms.
trimClosureNoDangerT :: Type -> Type
trimClosureNoDangerT tt
 = let	config	= Config { configMakeDanger	= False }
   in	trimClosureT' config Set.empty Set.empty tt



trimClosureT' config quant rsData tt
  = trace ("trimClosureT " % tt % "\n")
  $ let	tt_trimmed	= trimClosureT_trace config quant rsData tt
	tt_packFast	= packT tt_trimmed
			
	tt'		= trace ( "tt_trimmed  = " % tt_trimmed 	% "\n"
				% "tt_packFast = " % tt_packFast	% "\n\n")
		 		$ tt_packFast
		
    in	if tt' == tt
    		then tt'
		else trimClosureT' config quant rsData tt'


-- Here we are walking over the type looking for closures.
-- If we find one we pass it to trimClosureC to do the actual trimming.
trimClosureT_trace config quant rsData tt		
 = let down	= trimClosureT' config quant rsData
   in  case tt of
	TVar{}		-> tt
	TCon{}		-> tt

	TSum k _
	 | isClosureKind k	-> trimClosureC' config quant rsData tt
	 | otherwise		-> tt

	TApp t1 t2
	 | Just _	<- takeTFree tt
	 -> trimClosureC' config quant rsData tt

	 | otherwise	-> TApp (down t1) (down t2)

	TForall b k t
	 -> TForall b k (down t)
	
	TConstrain tBody (Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.filter (not . isTBot)
				$ Map.mapWithKey (trimClosureT_tt config quant rsData) crsEq

		crsMore'	= Map.filter (not . isTBot)
				$ Map.mapWithKey (trimClosureT_tt config quant rsData) crsMore

		crs'		= Constraints crsEq' crsMore' crsOther
	    in	addConstraints crs' tBody

	_ -> panic stage
		$ "trimClosureT: no match for " % show tt


-- | If this fetter is a closure constrain then trim the closure on the right,
--   otherwise return it unharmed.
trimClosureT_tt :: Config -> Set Type -> Set Type -> Type -> Type -> Type
trimClosureT_tt config quant rsData c1 c2
	| isClosure c1		= trimClosureC' config quant rsData c2
	| otherwise		= c2



-- Closure ----------------------------------------------------------------------------------------
-- | Trim a closure down to its interesting parts.
--   Here we are given a type which we know is a closure term.
--   It should not be embedded in a larger type (like as the closure of a function)
--   because we're not being passed the set of possibly quantified variables above us.
trimClosureC :: Closure -> Closure
trimClosureC cc
 = let	config	= Config { configMakeDanger	= True }
   in	trimClosureC' config Set.empty Set.empty cc

-- | Trim a closure, without producing TDanger terms.
trimClosureNoDangerC :: Closure -> Closure
trimClosureNoDangerC cc
 = let	config	= Config { configMakeDanger	= False }
   in	trimClosureC' config Set.empty Set.empty cc


trimClosureC' config quant rsData cc
 = let 	cc_trimmed	= trimClosureC_step config quant rsData cc
	cc_packed	= packT $ cc_trimmed
		
	cc'		= trace 
 				( "trimClosureC\n"	
 				% "    rsData   = " % rsData		% "\n"
				% "    closure  = " % cc		% "\n"
				% "    trimmed  = " % cc_trimmed	% "\n")
				cc_packed
   in	if cc' == cc
   	 then cc'
	 else trimClosureC' config quant rsData cc'

	
	
-- | Do a single step of closure trimming.
trimClosureC_step 
	:: Config	-- ^ Configuration saying how to do the trim.
	-> Set Type	-- ^ Variables that have been bound by a forall above us.
	-> Set Type	-- ^ Primary region variables of data types that we are inside.
	-> Closure 
	-> Closure
	
trimClosureC_step config quant rsData cc
 = let down	= trimClosureC' config quant rsData
   in  case cc of

	TVar k _
		-- Sanity: the types passed to trimClosureC should always be closures.
		| not $ isClosureKind k
		-> panic stage $ "trimClosureC: var doesn't have closure kind"

		-- If a variable has been bound by a forall above then it's not free
		-- and thus not part of the closure. We can safely leave it out.
		| Set.member cc quant 
		-> tEmpty

		-- A closure variable.
		-- TODO: why do we have the r /= c term?
		| otherwise		
		-> let	csDanger	= [makeTDanger' config r cc	
						| r	<- Set.toList rsData
						, r /= cc]
		   in	makeTSum kClosure $ cc : csDanger
	
	-- Trim the elements of a sum
	TSum{}	-> makeTSum kClosure 
		$  map down
		$  flattenTSum cc

	-- Trim constraints.
	TConstrain t Constraints { crsEq, crsMore }
	 -> let	t'		= trimClosureC' config quant rsData t

		crsEq'		= Map.filter (not . isTBot)
				$ Map.mapWithKey (\_ t2 -> trimClosureT' config quant rsData t2) crsEq

		crsMore'	= Map.filter (not . isTBot)
				$ Map.mapWithKey (\_ t2 -> trimClosureT' config quant rsData t2) crsMore

	    in	addConstraints (Constraints crsEq' crsMore' []) t'

	-- When we see a forall then add the bound variables to the remembered set.
	TForall b k t		
	 -> let Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k (UVar v)) quant
	    in	trimClosureC' config quant' rsData t


	-- Normalisation of TDanger terms --------------------------------------

	-- If configMakeTDanger is off then eat up any we see.
	-- TODO: make it so we don't produce the terms instead.
	TApp{}
	 | not $ configMakeDanger config
	 , Just (_, t1)		<- takeTFree cc
	 , Just (_, _)		<- takeTDanger t1
	 -> tEmpty
	
	
	-- TODO: This probably isn't the right place for these.
	--       They'd be better of as a part of crushT.
	
	-- TODO: This looks bogus.
	--       Danger terms just encode dangerousness, not reachability information also.
	-- 
	-- \${tag : %r11 $> %r12}
	--  => ${tag : %r11} + ${tag : %r12}
	TApp{}
	 | Just (tag, t1)	<- takeTFree   cc
	 , Just (t11, t12)	<- takeTDanger t1
	 , isRegion t11
	 , isRegion t12
	 -> makeTSum kClosure 
		[ makeTFreeBot tag t11
		, makeTFreeBot tag t12 ]
	
	 -- \${tag : t11 $> (t121 $> t122)}
	 --  => ${tag : t11 $> t121;  tag : t11 $> t122;  tag : t121 $> t122}
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
			  $ trimClosureC_t config tag quant rsData t12

	-- TODO: this shouldn't happen. 
	 | Just (_, t)		<- takeTFree cc
	 , isEffect t		
	 -> tEmpty
	
	 | Just (tag, t)	<- takeTFree cc
	 -> if isClosure t
		then makeTFreeBot tag $ down t
		else makeTFreeBot tag 
			$ makeTSum kClosure 
			$ trimClosureC_t config tag quant rsData t

	 | Just _		<- takeTDanger cc
	 -> cc
	
	_ -> panic stage
		$ "trimClosureC: no match for " % cc



-- | Trim a value type element of a closure.
trimClosureC_t :: Config -> Var -> Set Type -> Set Type -> Type -> [Type]
trimClosureC_t config tag quant rsData tt
 = let down	= trimClosureC_t config tag quant rsData
   in  case tt of
	-- If a variable has been bound by a forall above then it's not free
	-- and thus not part of the closure. We can safely leave it out.
	TVar{}
		| Set.member tt quant	-> []
		| otherwise		-> makeFreeDanger tag rsData tt

	-- Trim constraints.
	TConstrain tBody (Constraints crsEq crsMore crsOther)
	 -> let	crsEq'		= Map.filter (not . isTBot)
				$ Map.fromList 
				$ mapMaybe (trimClosureC_tt config quant rsData)
				$ Map.toList crsEq


		crsMore'	= Map.filter (not . isTBot)
				$ Map.fromList
				$ mapMaybe (trimClosureC_tt config quant rsData)
				$ Map.toList crsMore

		crs		= Constraints crsEq' crsMore' crsOther
	    	cBits		= down tBody
	    in	map (addConstraints crs) cBits
			
	-- When we see a forall then add the bound variables to the remembered set.
	TForall BNil _ t
	 -> trimClosureC_t config tag quant rsData t

	TForall b k t		
	 -> let	Just v	= takeVarOfBind b
		quant'	= Set.insert (TVar k (UVar v)) quant
	    in	trimClosureC_t config tag quant' rsData t
	
	-- Trim sums.
	TSum _ ts	-> concatMap down ts

	-- Plain constructors have no region variables or dangerous parts
	-- that we need to worry about.
	TCon{}		-> []

	-- When we enter into a data type then remember that we're under its primary region variable.
	-- If this region variable becomes mutable then all variables under it are dangerous.
	-- TODO: Won't the csDanger case be handled by the TVar case above?
	--       we don't need to create the danger terms twice.
	TApp{}
	 | Just (_, _, tsArgs)	<- takeTData tt
	 , t : _		<- tsArgs
	 , isRegion t
	 -> let rsData'	 = Set.insert t rsData
		vsArgs	 = freeVars tsArgs
		
		csHere   = concatMap (trimClosureC_t config tag quant rsData') tsArgs
		csDanger = map (makeTDanger t) 
				[TVar k (UVar v)
					| v <- Set.toList vsArgs
					, not $ Var.isCtorName v
					, let Just k = kindOfSpace $ varNameSpace v]
	    in  csHere ++ csDanger

	 -- data type has parameters but there is no primary region variable.
	 | Just (_, _, ts@(_ : _)) <- takeTData tt
	 -> concatMap down ts

	 -- data type has no parameters.
 	 | Just (_, _, [])	<- takeTData tt 
	 -> []
	
	
	 -- only the closure portion of a function type is material
	 -- TODO: this is wrong. we also need to look at the arg types incase they
	 --       consist of dangerous variables.
	 | Just (_, _, _, clo)	<- takeTFun tt
	 -> down clo

	 | Just (_, _)		<- takeTFree tt
	 -> [trimClosureC' config quant rsData tt]
		
	 | otherwise
	 -> []
	
	_ -> panic stage
		$ "trimClosureC_t: no match for (" % tt % ")"


-- | Trim a fetter on a type that is inside a closure term.
trimClosureC_tt 
	:: Config -> Set Type -> Set Type 
	-> (Type, Type) -> Maybe (Type, Type)
trimClosureC_tt config quant rsData (c1, c2)
 	| isClosure c1		= Just (c1, trimClosureC' config quant rsData c2)
	| isEffect c1		= Just (c1, c2)
	| otherwise		= Nothing
	

makeFreeDanger tag rsData t
	| Set.null rsData	= [t]

	| otherwise		
	= map (\r -> makeTDangerIfRegion tag r t) 
	$ Set.toList rsData

makeTDangerIfRegion tag r t
	| isRegion t	= makeTFreeBot tag t
	| otherwise	= makeTDanger r t


-- | If the config says we want TDanger terms then make one,
--   otherwise turn tEmpty.
makeTDanger' :: Config -> Region -> Type -> Closure
makeTDanger' config t1 t2
	| configMakeDanger config	= makeTDanger t1 t2
	| otherwise			= tEmpty