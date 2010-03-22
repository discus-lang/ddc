
-- | Extracting types from the graph.
module Type.Extract
	( extractType
	, extractTypeCid)
where
import Type.Plug
import Type.Context
import Type.Strengthen
import Type.Error
import Type.Trace
import Type.Class
import Type.State	
import Type.Util
import Type.Pretty
import Type.Plate
import Type.Exp
import Type.Util.Cut
import Util
import Shared.Error
import Shared.Var		(Var)
import qualified Shared.Var	as Var
import qualified Type.Util.PackFast	as PackFast
import qualified Data.Map	as Map
import qualified Data.Set	as Set
		
-----
stage	= "Type.Extract"
debug	= True
trace s	= when debug $ traceM s


-- | Extract a type from the graph and pack it into standard form.
--	This is the same as extractType, but takes the cid of the type instead of its var.
extractTypeCid 
	:: Bool 	-- ^ Whether to finalise the type as well.
	-> ClassId 	-- ^ Id of the class containing the type to extract.
	-> SquidM (Maybe Type)

extractTypeCid final cid
 = do	v	<- makeClassName cid
 	extractType final v


-- | Extract a type from the graph and pack it into standard form.
extractType 
	:: Bool 	-- ^ Whether to finalise the type as well. 
	-> Var 		-- ^ The variable of the type to extract.
	-> SquidM (Maybe Type)

extractType final varT
 = do	trace	$ "*** Scheme.extractType " % varT % "\n\n"

 	defs		<- gets stateDefs
	case Map.lookup varT defs of
	 -- If this var is in the defs table then it was imported from an external
	 --	interface (or is a generated constructor function), so we can just return it directly.
	 Just tt
	  -> do	trace 	$ "    def: " %> prettyTS tt % "\n"
		return $ Just tt
	
	 Nothing
	  -> {-# SCC "extractType_notDef" #-} 
	     extractType_findClass final varT

extractType_findClass final varT
 = do	
	-- try and find the equivalence class that corresponds to this var.
 	mCid	<- lookupVarToClassId varT
	case mCid of
	 Just cid	
	  -> extractType_fromClass final varT cid

	 -- If there is no equivalence class for this var then we've been asked for 
	 --	something that doens't exist in the graph. bad news, dude.
	 Nothing	
	  -> freakout stage
		 ("extractType: no classId defined for variable " % (varT, Var.varId varT) % "\n")
		$ return Nothing

extractType_fromClass final varT cid
 = do 	
	-- trace out all the equivalence classes reachable from this one, and build
	--	the initial type where each eq class is listed as its own fetter.
	--	If there is no fetter shown for a particular classid then it can be 
	--	asssumed to be Bot.
	--
	--	eg  *1	:- *1 = *2 -(!5 $6)> *3
	--		,  *2 :> Int %8
	--		,  *3 :> Unit
	--		,  !5 :> Read %8
	--
	trace	$ ppr " -- tracing type from the graph\n"
 	tTrace	<- {-# SCC "extract/trace" #-} traceType cid
	trace	$ "    tTrace:\n" 	%> prettyTS tTrace	% "\n\n"

	-- Pack the type into standard form.
	--	If we hit any loops through the value type portion of the
	--	graph then mark then with TError constructors.
	trace	$ ppr " -- packing into standard form\n"	
	let tPack	= PackFast.packType_markLoops tTrace

	-- Look for TErrors in the packed type
	let tsLoops	= [ (t1, t2) 
				| TError _ (TypeErrorLoop t1 t2) 
				<- collectTErrors tPack ]

	trace	$ "    tPack:\n" 	%> prettyTS tPack % "\n\n"
	
	if (isNil tsLoops)
	 -- no graphical data, ok to continue.
	 then extractType_more final varT cid tPack

	 -- we've got graphical data, add an error to the solver state and bail out.
	 else do
		let tsLoop1 : _	= tsLoops
	 	addErrors [ErrorInfiniteType 
				{ eVar	= varT 
				, eLoops	= [tsLoop1] }]

		return $ Just $ TError kValue (TypeErrorUnify [tTrace])

extractType_more final varT cid tPack
 = do	
	-- Strengthen more-than constraints. 
	-- In a type like
	--	fun 	:: ((a -(!e1)> b) -(!e2)> c)
	--		:- !e1 :> !e2
	--		,  !e2 :> !{ Read %r1; !e1 }
	--
	--	the constraint on !e2 is listed as :> !{ Read %r1; !e1} but it can only
	--	ever actually be = !{ Read %r1; !e1 } because it doesn't appear in a 
	--	contra-variant position in the shape of the type.
	trace	$ ppr " -- strengthening :> constraints\n"

	-- first work out what effect and closure vars are are represent parameters
	--	(are in a contravariant branch \/ to the left of a function arrow)
	let tsParam	
		= catMaybes
		$ map (\t -> case t of
				TClass kE cid | kE == kEffect   -> Just t
				TClass kC cid | kC == kClosure	-> Just t
				_				-> Nothing)
		$ slurpParamClassVarsT_constrainForm tPack
	
	tStrong	<- strengthenT (Set.fromList tsParam) tPack

	trace	$ "    tsParam   = " % tsParam	% "\n"
	trace	$ "    tStrong\n"
		%> prettyTS tStrong	% "\n\n"

	-- Trim closures
	trace	$ ppr " -- trimming closures\n"	
	let tTrim	
		| isClosure tStrong	= trimClosureC_constrainForm Set.empty Set.empty tStrong
		| otherwise		= trimClosureT_constrainForm Set.empty Set.empty tStrong

	trace	$ "    tTrim:\n" 	%> prettyTS tTrim % "\n\n"
	trace	$ ppr " -- done trimming\n"

	-- Cut loops through :> fetters in this type
	trace	$ ppr " -- cutting loops\n"
	let tCut	= cutLoopsT_constrainForm tTrim
	trace	$ "    tCut:\n" 	%> prettyTS tCut % "\n\n"
	
	let tCutPack	= PackFast.packType tCut
	trace	$ "    tCutPack:\n"	%> prettyTS tCutPack % "\n\n"

	extractType_final final varT cid tCutPack
	
extractType_final True varT cid tCutPack
 = do	
 	-- plug classIds with vars
 	tPlug	<- plugClassIds Set.empty tCutPack
	trace	$ "    tPlug:\n" 	%> prettyTS tPlug	% "\n\n"
 
	-- close off never-quantified effect and closure vars
 	quantVars	<- gets stateQuantifiedVars
 	let tFinal	=  finaliseT quantVars True tPlug
	
	trace	$ "    tFinal:\n" 	%> prettyTS tFinal	% "\n\n"
	extractType_reduce varT cid tFinal
	
extractType_final False varT cid tTrim
	= extractType_reduce varT cid tTrim

extractType_reduce varT cid tFinal
 = do	
	-- Reduce context
	classInst	<- gets stateClassInst

	let tReduced	
		= {-# SCC "extract/redude" #-}
		  reduceContextT classInst tFinal

	trace	$ " -- reducing context\n"
		% "    tReduced:\n" 	%> prettyTS tReduced % "\n\n"

	return	$ Just tReduced
