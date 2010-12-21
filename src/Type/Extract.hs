
-- | Extracting types from the graph.
module Type.Extract
	( extractType
	, extractTypeCid)
where
import Type.Plug
import Type.Strengthen
import Util
import DDC.Solve.Error
import DDC.Solve.State
import DDC.Solve.Trace
import DDC.Solve.Interface.Problem
import DDC.Main.Error
import DDC.Type.Simplify
import DDC.Type.Data
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map
import qualified Data.Set	as Set
		
stage		= "Type.Extract"
debug		= True
tracel  s 	= when debug $ traceM (s % nl)
tracell s 	= when debug $ traceM (s % nlnl)


-- | Extract a type from the graph and pack it into standard form.
--	This is the same as extractType, but takes the cid of the type instead of its var.
extractTypeCid 
	:: Bool 	-- ^ Whether to finalise the type as well.
	-> ClassId 	-- ^ Id of the class containing the type to extract.
	-> SquidM (Maybe Type)

extractTypeCid final cid
 = do	v	<- getCanonicalNameOfClass cid
	extractType final v


-- | Extract a type from the graph and pack it into standard form.
extractType 
	:: Bool 	-- ^ Whether to finalise the type as well. 
	-> Var 		-- ^ The variable of the type to extract.
	-> SquidM (Maybe Type)

extractType final varT
 = traceI
 $ do	tracel $ "*** extractType " %% varT % nl
	extractType_withEnv final varT

extractType_withEnv final varT
 = do	env	<- gets stateEnv
	let result
	 	-- If this var is in the defs table then it was imported from an external
	 	--	interface (or is a generated constructor function), so we can just return it directly.
		| Just (ProbDef _ _ tt)	<- Map.lookup varT (squidEnvDefs env)
		= return $ Just tt
		
		| Just ctorDef	<- Map.lookup varT (squidEnvCtorDefs env)
		= return $ Just $ ctorDefType ctorDef
		
		| otherwise
		= extractType_findClass final varT

	result


extractType_findClass final varT
 = do	-- try and find the equivalence class that corresponds to this var.
 	mCid	<- lookupCidOfVar varT
	case mCid of
	 Just cid	
	  -> extractType_fromClass final varT cid

	 -- If there is no equivalence class for this var then we've been asked for 
	 --	something that doens't exist in the graph. bad news, dude.
	 Nothing	
	  -> freakout stage
		 ("extractType: no classId defined for variable " % (varT, varId varT) % nl)
		$ return Nothing


extractType_fromClass final varT cid
 = do 	-- Trace ------------------------------------------
	-- Trace out all the equivalence classes reachable from this one.
 	tTrace	<- traceTypeAsSquid cid
	tracell	$ "-- traced" 		%! prettyTypeSplit tTrace

	-- For simple vars, applying the packer and flattener etc won't do anything,
	-- so skip all that stuff to avoid spamming the trace logs.
	case tTrace of
	 TVar{}	
	   -> extractType_final final varT cid tTrace

         -- common case for regions like  %1 :- Const %1
	 TConstrain TVar{} crs
	   | Map.null (crsEq crs)
	   , Map.null (crsMore crs)
	   -> extractType_final final varT cid tTrace

	 _ -> extractType_pack  final varT cid tTrace
	
	
extractType_pack final varT cid tTrace
 = do	-- Simplify --------------------------------------
	-- This is the new type simplifier, it's not finished yet.
	let tSimplified	= simplifyT tTrace 
	tracell	$ "-- simplified"	%! prettyTypeSplit tSimplified
	
	
	-- Drag ------------------------------------------
	-- drag local more-than constraints into their use sites.
	let tDragged	= dragT Set.empty tTrace
	tracell	$ "-- dragged" 		%! prettyTypeSplit tDragged

	-- Pack -------------------------------------------
	-- Pack the type into standard form. If we hit any loops through the
	-- value type portion of the graph then mark then with TError constructors.
	let tPack	= packAndMarkLoopsT tDragged

	-- Look for TErrors in the packed type. These mark recursive value type
	-- constraints, and we "cannot construct infinite types."
	let tsLoops	= [ (t1, t2) 	| TError _ (TypeErrorLoop t1 t2) 
					<- collectTErrors tPack ]

	tracell	$ "-- packed" 		%! prettyTypeSplit tPack
	
	if (isNil tsLoops)
	 -- no graphical data, ok to continue.
	 then extractType_more final varT cid tPack

	 -- we've got graphical data, add an error to the solver state and bail out.
	 else do
		let tsLoop1 : _	= tsLoops
	 	addErrors [ErrorInfiniteType 
				{ eVar	= varT 
				, eLoops	= [tsLoop1] }]

		return $ Just $ TError kValue TypeError


extractType_more final varT cid tPack
 = do	-- This needs to die -------------------------------------------------------
	-- Strengthen more-than constraints. 
	-- In a type like
	--	fun 	:: ((a -(!e1)> b) -(!e2)> c)
	--		:- !e1 :> !e2
	--		,  !e2 :> !{ Read %r1; !e1 }
	--
	--	the constraint on !e2 is listed as :> !{ Read %r1; !e1} but it can only
	--	ever actually be = !{ Read %r1; !e1 } because it doesn't appear in a 
	--	contra-variant position in the shape of the type.

	-- BREAKAGE: The result from strengthing above is
	--	fun 	:: ((a -(!e1)> b) -(!e2)> c)
	--		:- !e1 :> !e2
	--		,  !e2 =  !{ Read %r1; !e1 }
	--
	--	The trouble is that with higher order examples, the !e2 variable
	--	can end up in a parameter position.
	--	
	-- first determine what effect and closure vars are are represent parameters
	-- (are in a contravariant branch \/ to the left of a function arrow)
	let tsParam	
		= catMaybes
		$ map (\t -> case t of
				TVar kE (UClass cid) | kE == kEffect   -> Just t
				TVar kC (UClass cid) | kC == kClosure	-> Just t
				_				-> Nothing)
		$ slurpParamClassVarsT tPack
	
	tStrong	<- strengthenT (Set.fromList tsParam) tPack
	tracell	$ "-- strengthened"	%! prettyTypeSplit tStrong

	-----------------------------------------------------------------------
	let tHere	= tStrong	-- setting this to tPack makes Order5-2 test work,
					-- but breaks the Prelude.
--	let tHere	= tPack
	-----------------------------------------------------------------------

	-- Trim closures ------------------------------------------------------
	-- The extracted types tend to contain lots of boring closure information
	-- that isn't relavent to further stages. Trim that out now.
	let tTrim	
		| isClosure tHere	= trimClosureC tHere
		| otherwise		= trimClosureT tHere

	tracell	$ "-- trimmed closures"	%! prettyTypeSplit tTrim

	-- Cut loops ----------------------------------------------------------
	-- Effect constraints for recursive functions will tend to be recursive,
	-- but we can use some identities of :> to cut those loops.
	let tCut	= packT $ cutLoopsT tTrim
	tracell	$ "-- cut loops"	%! prettyTypeSplit tCut
	
	extractType_final final varT cid tCut

	
extractType_final True varT cid tCutPack
 = do	-- Plug classIds ------------------------------------------------------
	-- Replace meta variables in positions we're about to generalise with 
	-- real variable names from the corresponding equivalence classes.
 	tPlug		<- plugClassIds Set.empty tCutPack
	tracell	$ "-- plugged"  	%! prettyTypeSplit tPlug
 
	-- Finalise -----------------------------------------------------------
	-- Close off never-quantified effect and closure vars
 	quantVars	<- getsRef stateQuantifiedVars
 	let tFinal	= finaliseT quantVars True tPlug
	tracell	$ "-- finalised"  	%! prettyTypeSplit tFinal

	extractType_reduce varT cid tFinal
	
extractType_final False varT cid tTrim
 = 	extractType_reduce varT cid tTrim


extractType_reduce varT cid tFinal
 = do	-- Reduce context -----------------------------------------------------
	-- Remove type class constraints for instances that we know about.
	classInst	<- liftM squidEnvClassInst
			$  gets stateEnv

	let tReduced	= reduceContextT classInst tFinal
	tracell	$ "-- reduced context"	%! prettyTypeSplit tReduced

	return	$ Just tReduced

