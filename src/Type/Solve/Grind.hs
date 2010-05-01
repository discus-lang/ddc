
module Type.Solve.Grind
	(solveGrind)
where
import Type.Class
import Type.State
import Type.Exp
import Type.Builtin
import Type.Crush.Unify
import Type.Crush.Fetter
import Type.Crush.Effects
import Type.Crush.Shape
import Type.Crush.Proj
import Constraint.Exp
import Util
import DDC.Main.Error
import DDC.Var
import qualified DDC.Var.PrimId	as Var
import qualified Data.Set	as Set

-----
debug	= False
trace s = when debug $ traceM s

-- | Perform unification, resolve projections and grind out any available effects 
--	or fetters in the graph.
--
--	solveGrind may not be able to completely grind out all constraints because
--	the crushing of projections may require the projection function to be instantiated, 
--	triggering generalisation and requiring another grind.
--
--	If solveGrind returns no constraints, the grind succeeded and no crushable constructors
--	remain in the graph.
--
--	If solveGrind returns constraints, then they need to be processed before continuing the grind.
--	In this case the last constraint in the list will be another CGrind.
--
solveGrind 
	:: SquidM [CTree]

solveGrind
 = do	-- if there are already errors in the state then don't start the grind.
 	errs		<- gets stateErrors
	if isNil errs
	 then do 
		trace	$ ppr "-- Grind Start --------------------------------\n"
	 	cs	<- solveGrindStep
		trace	$ ppr "------------------------------------ Grind Stop\n"

		return cs

	 else return []
		
			
solveGrindStep 
 = do	trace	$ "\n"
 		% "*   solveGrindStep\n"

	 -- get the set of active classes
 	active	<- liftM Set.toList $ clearActive
	
	trace	$  "    active classes:\n"
		%> active	%  "\n"

	-- make sure all classes are unified
	progressUnify
		<- liftM or
		$  mapM crushUnifyClass active


	errors	<- gets stateErrors
	when (length errors > 0) $ do
		liftIO $ exitWithUserError [] errors

	-- grind those classes
	(progressCrush, qssMore)
		<- liftM unzip
		$  mapM grindClass active
 	
	-- split classes into the ones that made progress and the ones that
	--	didn't. This is just for the trace stmt below.
	let activeProgress	   = zip active progressCrush
	let classesWithProgress	   = [cid | (cid, True)  <- activeProgress]
	let classesWithoutProgress = [cid | (cid, False) <- activeProgress]
	
	trace	$ "*    classes that made progress:\n" 
		%> classesWithProgress % "\n"

	 	% "     classes without   progress:\n" 
		%> classesWithoutProgress % "\n"

	let qsMore	= concat qssMore
	errs		<- gets stateErrors
		
	trace	$ ppr "\n"
	let next
		-- if we've hit any errors then bail out now
		| not $ null errs
		= return []

		-- if we've crushed a projection and ended up with more constraints
		--	then stop grinding for now, but ask to be resumed later when
		--	new constraints are added.
		| not $ null qsMore
		= return (qsMore ++ [CGrind])
		
		-- if we've made progress then keep going.
		| progressUnify || or progressCrush
		= solveGrindStep 
		
		-- no progress, all done.
		| otherwise
		= return []
		
	next


grindClass :: ClassId -> SquidM (Bool, [CTree])
grindClass cid
 = do	Just c	<- lookupClass cid
	grindClass2 cid c

grindClass2 cid c@(ClassForward cid')
	= grindClass cid'
	
grindClass2 cid c@(ClassNil)
	= return (False, [])
	 	
-- type nodes
grindClass2 cid c@(Class	
			{ classType 		= mType
			, classKind		= k 
			, classFetterSources	= fs_src})
 = do	
	-- if a class contains an effect it might need to be crushed
	progressCrushE	
		<- case k of
			kE | kE == kEffect	-> crushEffectC cid
			_			-> return False

	-- try and crush other fetters in this class
	progressCrush
		<- case fs_src of
			[]	-> return False
			_	-> crushFetterC cid
	
	return	( progressCrushE || progressCrush
		, [])
	
-- fetter nodes
grindClass2 cid c@(ClassFetter { classFetter = f })
 = do
	-- crush projection fetters
	qsMore	<- case f of
			FProj{}	-> crushProjClassT cid
			_	-> return Nothing
			
	let progressProj
		= isJust qsMore
		
	-- crush shape fetters
	let isFShape b
		= case b of
			Var.FShape _	-> True
			_		-> False

	progressShape
		<- case f of
			FConstraint v _
			 | VarIdPrim pid	<- varId v
			 , isFShape pid		-> crushShape cid
			_			-> return False
		
	-- crush other fetters
	progressCrush
		<- crushFetterC cid
		
	return	( progressProj || progressShape || progressCrush
		, fromMaybe [] qsMore )
