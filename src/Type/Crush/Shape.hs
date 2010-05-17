-- | Handles crushing of shape constraints.

module Type.Crush.Shape
	(crushShape)
where
import Type.Feed
import Type.Location
import Type.Exp
import Type.Builtin
import Type.State
import Type.Class
import Type.Crush.Unify
import Shared.VarPrim
import Util
import qualified Data.Map		as Map
import qualified Data.Set		as Set

debug	= False
trace s	= when debug $ traceM s

-- | Try and crush the Shape constraint in this class.
--   If any of the nodes in the constraint contains a type constructor then add a similar constructor
--   to the other nodes and remove the constraint from the graph.
--
--   returns whether we managed to crush this fetter.
--
crushShape :: ClassId -> SquidM Bool
crushShape cidShape
 = do 	
	-- Grab the Shape fetter from the class and extract the list of cids to be merged.
	Just shapeC@ClassFetter
		{ classFetter	= fShape@(FConstraint v shapeTs)
		, classSource	= srcShape }	
			<- lookupClass cidShape

	-- All the cids constrained by the Shape constraint.
	let mergeCids	= map (\(TClass k cid) -> cid) shapeTs

	trace	$ "*   Crush.crushShape " 	% cidShape 	% "\n"
		% "    fetter      = "	 	% fShape	% "\n"
		% "    mergeCids   = "		% mergeCids 	% "\n"

 	-- Make sure that all the classes to be merged are unified.
	--	We're expecting a maximum of one constructor per class queue.
 	mapM crushUnifyInClass mergeCids
 
	-- Lookup all the nodes.
 	csMerge		<- liftM (map (\(Just c) -> c)) 
 			$  mapM lookupClass mergeCids

	-- See if any of the nodes contain information that needs
	--	to be propagated to the others.
	let mData	= map (\c -> case classType c of
					Just t@NApp{}	-> Just t
					Just t@NCon{}	-> Just t
					_		-> Nothing)
			$ csMerge
	
	trace	$ "    classTypes   = " % map classType  csMerge % "\n"
	
	-- If we have to propagate the constraint we'll use the first constructor as a template.
	let mTemplate	= takeFirstJust mData
	trace	$ "    mData       = "	% mData		% "\n"
		% "    mTemplate    = "	% mTemplate	% "\n"
		% "\n"

	let result
		-- If the constrained equivalence class is of effect or closure kind
		--	then we can just delete the constraint
		| TClass k _ : _	<- shapeTs
		, k == kClosure || k == kEffect
		= do	delClass cidShape
			return True

		-- none of the nodes contain data constructors, so there's no template to work from
		| Nothing	<- mTemplate
		= return False
		
		-- we've got a template
		--	we can now merge the sub-classes and remove the shape constraint.
		| Just tTemplate	<- mTemplate
		= do	crushShape2 cidShape fShape srcShape tTemplate csMerge
			delClass cidShape
			
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
 = do	kind	<- kindOfCid cid1

	-- shape fetters don't constrain regions.
	if kind == kRegion
	 then	return ()
	 else do
		let ts	= zipWith TClass (repeat kind) cids
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
	| Class { classType = Just node }	<- cMerge
	, isNBot node
	= do	
		tPush	<- freshenNode tTemplate
		trace 	$ "  - merge class\n"
			% "    tPush = " % tPush	% "\n"		

		addToClass (classId cMerge) srcShape (classKind cMerge) tPush
		return $ Just tPush		

	-- If adding the template will result in a type error then add the error to
	-- 	the solver state, and return Nothing.
	--	This prevents the caller, crushShape2 recursively adding more errornous
	--	Shape constraints to the graph.
	--	
	| Class { classType = Just t}		<- cMerge
	= if isShallowConflict t tTemplate
	   then	
	    do	let cError	= cMerge { classTypeSources = (tTemplate, srcShape) : classTypeSources cMerge }
	 	addErrorConflict (classId cError) cError
		return Nothing

	   else return (Just t)


-- | replace all the free vars in this type with new ones
freshenNode :: Node -> SquidM Node
freshenNode node
 = do	let cidsFree	= Set.toList $ cidsOfNode node
 	cidsFresh	<- mapM freshenCid cidsFree
	let sub		= Map.fromList $ zip cidsFree cidsFresh
	return	$ subNodeCidCid sub node


freshenCid :: ClassId -> SquidM ClassId
freshenCid cid
 = do	Just Class { classKind = k }	
 		<- lookupClass cid
 
 	cid'	<- allocClass k
	updateClass cid'
		(classInit cid' k)
		{ classType = Just nBot }

	return	cid'
 
