
-- | Extracting types from the graph.
module Type.Extract
	( extractType)
where

import Type.Check.GraphicalData
import Type.Plug
import Type.Context
import Type.Port
import Type.Error
import Type.Trace
import Type.Class
import Type.State	
import Type.Util
import Type.Pretty
import Type.Plate
import Type.Exp

import qualified Shared.Var	as Var
import Shared.Var		(Var)
import Shared.Error
import Util

import qualified Data.Map	as Map
import Data.Map			(Map)
		
import qualified Data.Set	as Set
import Data.Set			(Set)
		
-----
stage	= "Type.Extract"
debug	= True
trace s	= when debug $ traceM s


-- | Extract a type from the graph and pack it into standard form.
extractType 
	:: Bool 	-- ^ Whether to finalise the type as well. 
			--   After all constraints are added to the graph we can treat
			--	empty effect and closure classes as TBot, because nothing
			--	more can ever be added to them.
	
	-> Var 		-- ^ The variable of the type to extract.

	-> SquidM (Maybe Type)

extractType final varT
 = do	
 	trace	$ "*** Scheme.extractType " % varT % "\n"
		% "\n"

 	defs		<- gets stateDefs
--	varT'		<- sinkVar varT
--	quantKinds	<- gets stateQuantifiedVars

	let result
		-- If this var is in the defs table then it was imported from an external
		--	interface (or is a generated constructor function), so we can just
		--	return it directly.
		| Just tt	<- Map.lookup varT defs
		= do	trace 	$ "    def: " %> prettyTS tt % "\n"
			return $ Just tt
		
		-- Otherwise we'll have to trace it out from the graph.
		| otherwise
		= {-# SCC "extractType" #-} 
		  extractType_findClass final varT

	result


extractType_findClass final varT
 = do	
	-- try and find the equivalence class that corresponds to this var.
 	mCid	<- lookupVarToClassId varT
	case mCid of
	 Just cid	-> extractType_fromClass final varT cid

	 -- If there is no equivalence class for this var then we've been asked for 
	 --	something that doens't exist in the graph. bad news, dude.
	 Nothing	
	  -> do	graph			<- gets stateGraph
	  	let varToClassId	=  graphVarToClassId graph
	  	freakout stage
		 	("extractType: no classId defined for variable " % (varT, Var.bind varT)		% "\n")
-- debug		% " visible vars = " % (map (\v -> (v, Var.bind v)) $ Map.keys varToClassId)		% "\n")
			$ return Nothing


extractType_fromClass final varT cid
 = do 	
	-- trace out all the equivalence classes reachable from this one, and build
	--	the initial type where each eq class is listed as its own fetter.
	--	If there is no fetter shown for a particular classid then it can be 
	--	asssumed to be Bot.
	--
	--	eg  *1	:- *1 = *2 -(!5 $6)> *3
	--		,  *2 = Int %8
	--		,  *3 = Unit
	--		,  !5 = Read %8
	--
	trace	$ ppr " -- tracing type from the graph\n"

 	tTrace	<- liftM sortFsT 	
		$ {-# SCC "extract/trace" #-} 
		  traceType cid

	trace	$ "    tTrace:\n" 	%> prettyTS tTrace	% "\n\n"

	-- Check if the data portion of the type is graphical.
	--	If it is then it'll hang packType when it tries to construct an infinite type.
	let cidsDataLoop	
		= checkGraphicalDataT tTrace

	trace	$ "    cidsDataLoop:\n" %> cidsDataLoop % "\n\n"

	if (isNil cidsDataLoop)
	 -- no graphical data, ok to continue.
	 then extractType_pack final varT cid tTrace

	 -- we've got graphical data, add an error to the solver state and bail out.
	 else do
	 	addErrors [ErrorInfiniteTypeClassId {
	 			eClassId	= head cidsDataLoop }]

		return $ Just $ TError KData [tTrace]


extractType_pack final varT cid tTrace
 = do	
	-- Cut loops through the effect and closure portions of this type.
	let tCutLoops	
		= {-# SCC "extract/cut" #-} 
		  cutLoopsT tTrace

	trace	$ "    tCutLoops:\n" 	%> prettyTS tCutLoops % "\n\n"

	-- Pack the type into standard form.
	trace	$ ppr " -- packing into standard form\n"	
	let tPack	
		= {-# SCC "extract/pack" #-} 
		  packType tCutLoops

	trace	$ "    tPack:\n" 	%> prettyTS tPack % "\n\n"

	-----
	-- More-ify fetters
	-- 	in a type like
	--
	--	fun 	:: ((a -(!e1)> b) -(!e2)> c)
	--		:- !e1 :> !e2
	--		,  !e2 :> !{ Read %r1; !e1 }
	--
	--	the constraint on !e2 is listed as :> !{ Read %r1; !e1} but it can only
	--	ever actually be = !{ Read %r1; !e1 } because it doesn't appear in a 
	--	contra-variant position in the shape of the type.
	--
	--	BUGS: must also not be in the environment.
	
	trace	$ ppr " -- dropping :> on non-contravariant effect and closure cids\n"
	

	-- first work out what effect and closure vars are in contra-variant branches
	let contraTs	= catMaybes
			$ map (\t -> case t of
					TClass KEffect cid	-> Just t
					TClass KClosure cid	-> Just t
					_			-> Nothing)
			$ slurpContraClassVarsT tPack
	
	tDeMore		<- dropFMoresT (Set.fromList contraTs) tPack
	trace	$ "    contraTs = " % contraTs	% "\n"
	trace	$ "    tDeMore\n"
		%> prettyTS tDeMore	% "\n\n"

	-----
	-- Trim closures
	let tTrim	= 
		case kindOfType tDeMore of
			KClosure	-> trimClosureC Set.empty tDeMore
			_		-> trimClosureT Set.empty tDeMore

	trace	$ " -- trimming closures\n"
		% "    tTrim:\n" 	%> prettyTS tTrim % "\n\n"

	let tTrimPack	
		= {-# SCC "extract/pack_trim" #-}
		  packType tTrim

	trace	$ "    tTrimPack:\n"	%> prettyTS tTrimPack % "\n\n"


	extractType_final final varT cid tTrimPack
	

extractType_final True varT cid tTrim
 = do	
 	-- plug classIds with vars
 	tPlug		<- plugClassIds [] tTrim
	trace	$ "    tPlug:\n" 	%> prettyTS tPlug	% "\n\n"
 
	-- close off never-quantified effect and closure vars
 	quantVars	<- gets stateQuantifiedVars
 	let tFinal	=  finaliseT quantVars True tPlug
	
	trace	$ "    tFinal:\n" 	%> prettyTS tFinal	% "\n\n"
	extractTypeC2 varT cid tFinal
	
extractType_final False varT cid tTrim
	= extractTypeC2 varT cid tTrim

extractTypeC2 varT cid tFinal
 = do	
	-- Reduce context
	classInst	<- gets stateClassInst

	let tReduced	
		= {-# SCC "extract/redude" #-}
		  reduceContextT classInst tFinal

	trace	$ " -- reducing context\n"
		% "    tReduced:\n" 	%> prettyTS tReduced % "\n\n"

	return	$ Just tReduced



{--
addTMores :: Type -> SquidM Type
addTMores tt	= transformTM addTMores1 tt

addTMores1 tt
	| TVar k var	<- tt
	= do	quantKinds	<- gets stateQuantifiedVars
	 	var'		<- sinkVar var
		
		let result
			| Just (k, Just tMore)	<- Map.lookup var' quantKinds
			= do	tMore'	<- addTMores tMore
			  	return	$ TFetters [FMore (TVar k var') tMore'] (TVar k var')

			| otherwise
			= return tt

		result
		 
	| otherwise
	= return tt
-}
