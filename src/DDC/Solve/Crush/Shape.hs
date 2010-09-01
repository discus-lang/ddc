{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

module DDC.Solve.Crush.Shape
	(crushShapeInClass)
where
import Type.Feed
import Shared.VarPrim
import DDC.Solve.Location
import DDC.Solve.Crush.Unify
import DDC.Solve.State
import DDC.Type
import Util
import qualified Data.Map		as Map
import qualified Data.Set		as Set

debug	= False
trace s	= when debug $ traceM s

-- | Try and crush the Shape constraint in this class.
--   If any of the nodes in the constraint contains a type constructor then add a similar constructor
--   to the other nodes and remove the constraint from the graph.
--
--   Returns `True` if we've made progress with this constraint.
--
crushShapeInClass :: ClassId -> SquidM Bool
crushShapeInClass cidShape
 = do 	
	-- Grab the Shape fetter from the class and extract the list of cids to be merged.
	Just ClassFetter
		{ classFetter	= fShape@(FConstraint _ shapeTs)
		, classSource	= srcShape }	
			<- lookupClass cidShape

	-- All the cids constrained by the Shape constraint.
	let mergeCids	= map (\(TVar _ (UClass cid)) -> cid) shapeTs

	trace	$ vcat
		[ "-- Crush.shape " 		% cidShape
		, "   fetter      = "	 	% fShape
		, "   mergeCids   = "		% mergeCids
		, blank ]

 	-- Ensure that all the classes to be merged are unified.
	-- We need this because resolving shape constraints can add more nodes
	-- to an equivalence class. So if the grinder is just calling crushShapeInClass
	-- on a set of cids the unifier won't get to run otherwise.
 	mapM crushUnifyInClass mergeCids
 
	-- Lookup all the classes that are being constrained by the Shape.
 	clsMerge	<- liftM (map (\(Just cls) -> cls)) 
 			$  mapM lookupClass mergeCids

	-- See if any of the nodes contain information that needs
	--	to be propagated to the others.
	let templates	= map (\c -> case classUnified c of
					Just t@NApp{}	-> Just t
					Just t@NCon{}	-> Just t
					_		-> Nothing)
			$ clsMerge
	
	trace	$ vcat
		[ "-- Crush.shape " 	% cidShape % " (unify done)"
		, "   unified      = " 	% map classUnified  clsMerge ]
	
	-- If we have to propagate the constraint we'll use the first constructor as a template.
	let mTemplate	= takeFirstJust templates
	trace	$ "   templates    = "	% templates	% "\n"
		% "   mTemplate    = "	% mTemplate	% "\n"

	let result
		-- TODO: For region classes, check if they're material or not before merging.

		-- Effects and closures are always immaterial, so we just merge the classes.
		| clsMerge1 : _	<- clsMerge
		, kind		<- classKind clsMerge1
		, kind == kClosure || kind == kEffect
		= do	trace $ ppr "    -- eff/clo are immaterial so merging classes\n\n"
			mergeClasses 	$ map classId clsMerge
			delMultiFetter cidShape
			return True

		-- None of the nodes contain data constructors, so there's no template to work from.
		-- However, something might be unified in later, so activiate ourselves
		-- so the grinder calls us again on the next pass.
		| Nothing	<- mTemplate
		= do	trace $ ppr "   -- no template, reactivating\n\n"
			activateClass cidShape
			return False
		
		-- we've got a template
		--	we can now merge the sub-classes and remove the shape constraint.
		| Just tTemplate	<- mTemplate
		= do	crushShapeWithTemplate cidShape fShape srcShape tTemplate clsMerge
			delMultiFetter cidShape
			return True
	
	result		


-- | Given come classes constrained by a Shape, and a template node giving the required
--   shape, constrain each of the classes so they match the template.
crushShapeWithTemplate
	:: ClassId		-- ^ cid of the Shape fetter being crushed.
	-> Fetter		-- ^ the Shape fetter being crushed.
	-> TypeSource		-- ^ source of the shape fetter.
	-> Node			-- ^ the template node.
	-> [Class]		-- ^ the classes being constrained by the Shape fetter.
	-> SquidM ()

crushShapeWithTemplate cidShape fShape srcShape tTemplate csMerge
 = do
 	trace  	( "-- Crush.shape (got template)\n"
	 	% "   cidShape  = " % cidShape		% "\n"
		% "   fShape    = " % fShape		% "\n"
		% "   srcShape  = " % srcShape		% "\n"
		% "   tTemplate = " % tTemplate	% "\n")

	-- the source to use for the new constraints.
	let srcCrushed	= TSI $ SICrushedFS cidShape fShape srcShape

	-- look at each of the classes being constrained, and if they don't already
	-- have a constructor that maches the template, then push one in.
	mtsPushed	<- mapM (pushTemplate tTemplate srcCrushed) csMerge
	
	case sequence mtsPushed of
	 -- get the cids that should be recursively constrained.	
	 Just tsPushed
	  -> do	let takeRec tt
		     = case tt of
			NApp t1 t2	-> [t1, t2]
			_		-> []
					
		let tssMerged	= map takeRec tsPushed
		let tssMergeRec	= transpose tssMerged
		
		trace	$ "    tssMergeRec = " % tssMergeRec		% "\n"

		-- add shape constraints to constraint the args as well
		mapM_ (addShapeFetter srcCrushed) tssMergeRec


	 -- If adding the template to the class would result in a type error
	 -- then stop now, as we don't want to change the graph anymore if it
	 -- already has errors. We'll get an invalid instance error for
	 -- the un-crushed shape constraint anyway.
	 _ 	-> return ()
		

-- | Add a template type to a class.
pushTemplate 
	:: Node			-- the template type
	-> TypeSource		-- the source of the shape fetter doing the pushing
	-> Class		-- the class to push the template into.
	-> SquidM (Maybe Node)
	
pushTemplate tTemplate srcShape cMerge

	-- if this class does not have a constructor then we 
	--	can push the template into it.
	| Class { classUnified = Just node }	<- cMerge
	, isNBot node
	= do	
		tPush	<- freshenNode srcShape tTemplate
		trace 	$ "  -- merge class\n"
			% "    tPush = " % tPush	% "\n"		

		addNodeToClass (classId cMerge) (classKind cMerge) srcShape tPush
		return $ Just tPush		

	-- If adding the template will result in a type error then add the error to
	-- 	the solver state, and return Nothing.
	--	This prevents the caller, crushShape2 recursively adding more errornous
	--	Shape constraints to the graph.
	--	
	| Class { classUnified = Just t }	<- cMerge
	= if isShallowConflict t tTemplate
	   then	
	    do	let cError	= cMerge { classTypeSources = (tTemplate, srcShape) : classTypeSources cMerge }
	 	addErrorConflict (classId cError) cError
		return Nothing

	   else return (Just t)


-- | Replace all the free cids in this node with fresh ones.
freshenNode :: TypeSource -> Node -> SquidM Node
freshenNode src node
 = do	let cidsFree	= Set.toList $ cidsOfNode node
 	cidsFresh	<- mapM (freshenCid src) cidsFree
	let sub		= Map.fromList $ zip cidsFree cidsFresh
	return	$ subNodeCidCid sub node


-- | Allocate a new class with the same kind as this one.
freshenCid :: TypeSource -> ClassId -> SquidM ClassId
freshenCid src cid
 = do	Just Class { classKind = k }	
 		<- lookupClass cid
 
 	cid'	<- allocClass k src
	updateClass False cid'
		(emptyClass k src cid')
		{ classUnified = Just nBot }

	return	cid'

 
-- | Add a shape fetter to constrain all these classes.
--   TODO: We're treating all regions as strongly material, until we
--         can work out the real materiality. 
addShapeFetter :: TypeSource -> [ClassId] -> SquidM ()
addShapeFetter src cids@(cid1 : _)
 = do	kind	<- kindOfClass cid1

	-- shape fetters don't constrain regions.
	if kind == kRegion
	 then	return ()
	 else do
		let ts	= zipWith 
				(\k c -> TVar k (UClass c)) 
				(repeat kind) cids

		addFetter src (FConstraint (primFShape (length ts)) ts)
		return ()
