
module Type.Solve.Grind
	(solveGrind)
where
import DDC.Solve.Crush.Unify
import DDC.Solve.Crush.Shape
import DDC.Solve.Crush.Effects
import DDC.Solve.Crush.Fetter
import DDC.Solve.Crush.Proj
import Constraint.Exp
import Util
import DDC.Main.Error
import DDC.Solve.State
import DDC.Type.Kind
import DDC.Type.Exp
import DDC.Var
import qualified DDC.Var.PrimId	as Var
import qualified Data.Set	as Set

debug	= True
stage	= "Type.Solve.Grind"
trace s = when debug $ traceM s

-- | Perform unification, resolve projections and grind out any available effects 
--	or fetters in the graph.
--
--	solveGrind may not be able to completely grind out all constraints because
--	the crushing of projections may require the projection function to be
--	instantiated, triggering generalisation and requiring another grind.
--
--	If solveGrind returns no constraints, the grind succeeded and no crushable
--	constructors remain in the graph.
--
--	If solveGrind returns constraints, then they need to be processed before
--	continuing the grind. In this case the last constraint in the list will
--	be another CGrind.
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
 		% "-- Solve.grind step\n"

	 -- get the set of active classes
 	active	<- liftM Set.toList $ clearActive

	trace	$ "   active classes:\n"
		%> active	%  "\n"

	solveGrindStepWithActive active
	

solveGrindStepWithActive active
 | null active	= return []
 | otherwise
 = do
	-- make sure all classes are unified
	progressUnify
		<- mapM crushUnifyInClass active

	errors	<- gets stateErrors
	when (length errors > 0) $ do
		liftIO $ exitWithUserError [] errors

	-- grind those classes
	(progressCrush, qssMore)
		<- liftM unzip
		$  mapM crushClass active

	-- The new constraints
	let qsMore	= concat qssMore
 	
	-- split classes into the ones that made progress and the ones that
	--	didn't. This is just for the trace stmt below.
	let activeUnify		   = zip active progressUnify
	let activeProgress	   = zip active progressCrush
	let classesWithProgress	   = Set.fromList [cid | (cid, True)  <- activeUnify ++ activeProgress]
	let classesWithoutProgress = Set.fromList active `Set.difference` classesWithProgress
	
	trace	$ "   classes that made progress:\n" 
		%> classesWithProgress % "\n"

		% "   classes without progress\n" 
		%> classesWithoutProgress % "\n"

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
		| or progressUnify || or progressCrush
		= solveGrindStep 
		
		-- no progress, all done.
		| otherwise
		= return []
		
	next


-- | Try to crush a class in the graph
crushClass :: ClassId -> SquidM (Bool, [CTree])
crushClass cid
 = lookupClass cid >>= \(Just cls)
 -> case cls of
	ClassUnallocated{}	
	 -> panic stage $ "crushClass: ClassUnallocated{} " % cid
	
	ClassForward _ cid'
	 -> panic stage $ "crushClass: the cids to grind should already be canonicalised."

	-- unifiable classes
	(Class
		{ classUnified 	= mType
		, classKind	= k 
		, classFetters	= fsSrcs})

	 | isEffectKind k
	 -> do	progress_effect	<- crushEffectsInClass cid
		progress_fetter	<- crushFettersInClass cid
		return	( progress_effect || progress_fetter
			, [])
		
	 | isClosureKind k
	 -> 	return	( False
			, [])
	
	 | isRegionKind k
	 ->	return 	( False
			, [])
	
	 | otherwise
	 -> do	progress_unify	<- crushUnifyInClass cid
		progress_fetter	<- crushFettersInClass cid
		return	( progress_unify || progress_fetter
			, [])

	-- fetter classes
	ClassFetterDeleted{}
	 -> 	return 	( False
			, [])

	ClassFetter { classFetter = f }
	 | FProj{}	<- f
	 -> do	mQsMore 	<- crushProjInClass cid
		return	( isJust mQsMore
			, join $ maybeToList mQsMore)
		
	 | FConstraint v _	<- f
	 , VarIdPrim pid	<- varId v
	 , Var.FShape _		<- pid
	 -> do	progress_shape	<- crushShapeInClass cid
		return	( progress_shape
			, [])
		
