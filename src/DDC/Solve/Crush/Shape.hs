{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns #-}

-- | Crush any shape constraints in this class.
module DDC.Solve.Crush.Shape
	(crushShape)
where
import Type.Feed
import Type.Location
import Shared.VarPrim
import DDC.Solve.Crush.Unify
import DDC.Solve.State
import DDC.Type
import Util
import qualified Data.Map		as Map
import qualified Data.Set		as Set

debug	= True
trace s	= when debug $ traceM s

-- | Try and crush the Shape constraint in this class.
--   If any of the nodes in the constraint contains a type constructor then add a similar constructor
--   to the other nodes and remove the constraint from the graph.
--
--   Returns `True` if we've made progress with this constraint.
--
crushShape :: ClassId -> SquidM Bool
crushShape cidShape
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
	-- TODO: To avoid this check we might want to call the crusher after the unifier.
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
		-- If the constrained equivalence class is of effect or closure kind
		-- then we can just delete the constraint. 
		-- BUGS: This is wrong. We're supposed to check if it's manifest.
		| TVar k (UClass _) : _	<- shapeTs
		, k == kClosure || k == kEffect
		= do	trace $ ppr "    -- is eff/clo so deleting constraing (BUGS)\n\n"
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
		= do	crushShape2 cidShape fShape srcShape tTemplate clsMerge
			delMultiFetter cidShape
			return True
	
	result		


crushShape2 
	:: ClassId		-- the classId of the fetter being crushed
	-> Fetter		-- the shape fetter being crushed
	-> TypeSource		-- the source of the shape fetter
	-> Node			-- the template type"
	-> [Class]		-- the classes being merged
	-> SquidM ()

crushShape2 cidShape fShape srcShape tTemplate csMerge
 = do
 	trace  	( "*   Crush.crushShape2\n"
	 	% "    cidShape  = " % cidShape		% "\n"
		% "    fShape    = " % fShape		% "\n"
		% "    srcShape  = " % srcShape		% "\n"
		% "    tTemplate = " % tTemplate	% "\n")

	let srcCrushed	= TSI $ SICrushedFS cidShape fShape srcShape

	-- push the template into classes which don't already have a ctor
	mtsPushed	<- mapM (pushTemplate tTemplate srcCrushed) csMerge
	
	let result
		| Just tsPushed		<- sequence mtsPushed
		= do	
			let takeRec tt 
				| NApp t1 t2		<- tt
				= [t1, t2]
				
				| otherwise
				= []
					
			let tssMerged	= map takeRec tsPushed
			let tssMergeRec	= transpose tssMerged
		
			trace	( "    tssMergeRec = " % tssMergeRec		% "\n")

			-- add shape constraints to constraint the args as well
			mapM_ (addShapeFetter srcCrushed) tssMergeRec

		  	return ()
		
		-- If adding the template to another class would result in a type error
		--	then stop now. We don't want to change the graph anymore if it
		--	already has errors.
		| otherwise
		= return ()

	result

addShapeFetter :: TypeSource -> [ClassId] -> SquidM ()
addShapeFetter src cids@(cid1 : _)
 = do	kind	<- kindOfClass cid1

	-- shape fetters don't constrain regions.
	if kind == kRegion
	 then	return ()
	 else do
		let ts	= zipWith (\k c -> TVar k (UClass c)) (repeat kind) cids
		addFetter src (FConstraint (primFShape (length ts)) ts)
		return ()

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


-- | replace all the free vars in this type with new ones
freshenNode :: TypeSource -> Node -> SquidM Node
freshenNode src node
 = do	let cidsFree	= Set.toList $ cidsOfNode node
 	cidsFresh	<- mapM (freshenCid src) cidsFree
	let sub		= Map.fromList $ zip cidsFree cidsFresh
	return	$ subNodeCidCid sub node


freshenCid :: TypeSource -> ClassId -> SquidM ClassId
freshenCid src cid
 = do	Just Class { classKind = k }	
 		<- lookupClass cid
 
 	cid'	<- allocClass k src
	updateClass cid'
		(emptyClass k src cid')
		{ classUnified = Just nBot }

	return	cid'
 
