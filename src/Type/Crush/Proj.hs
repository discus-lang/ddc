
-- | Crush put projection constraints.
--	We need to wait until the type being projected resolves to something
--	with an outermost constructor, lookup the type of the projection
--	instance function and add it to the graph.
--
--	We also record how each projection constraint was resolved in the
--	projection resolution table. We need this information when converting
--	the desugared program to the core language.
--
module Type.Crush.Proj
	(crushProjInClass)
where
import Type.Location
import Type.Error
import Type.Dump		()
import Constraint.Exp
import Util
import DDC.Solve.Trace
import DDC.Solve.Walk
import DDC.Solve.State
import DDC.Type
import DDC.Var
import qualified Data.Map	as Map

debug	= False
trace s	= when debug $ traceM s


-- | Crush out projection constraints from an equivalence class.
--	This function returns new constraints containing the type
--	of the projection instance function.
--
crushProjInClass
	:: ClassId 			-- ^ Class containing the projection constraint to crush.
	-> SquidM (Maybe [CTree])	-- ^ New constraints to add to the graph.

crushProjInClass cid
 = do	-- Grab the projection constraint from the graph node.
 	Just clsProj	<- lookupClass cid
	
	let ClassFetter 
	 	{ classFetter	= fProj@(FProj _ _ (TVar _ (UClass cidObjRoot)) _)
		, classSource	= src }	
			= clsProj

	trace	$ "--  crushProjInClass\n"
		% "    classId of fetter  (cidT)        = " % cid		% "\n"
		% "    fetter                           = " % fProj		% "\n"
	
	-- The "object type" is the type we're using to determine which projection
	--	instance function to use. Starting from the root of this type, 
	--	walk down its left spine to see if there is a tycon at the front.
	--	If there is, then return the classId of the node containing this 
	--	tycon.
	clsObj <- getClassDownLeftSpine cidObjRoot
	
	case clsObj of
	 -- The unifier needs to unify the nodes in this class before we know
	 --	what the object type will be.
	 Class { classUnified = Nothing }
	  -> do	trace 	$ ppr "  * Object class not unified yet, deferring.\n"
			% clsObj % "\n\n"

		-- Reactivate the class so we get called again during the next grind.
		activateClass cid
	 	return Nothing

	 -- Ok, we've got an object type, carry on.
	 Class { classUnified = Just nObj }
	  -> do	Just tObj	<- takeShallowTypeOfCidAsSquid (classId clsObj)
		trace	$ "    object type (tObj)               = " % tObj	% "\n"

		-- Grab the map of projection dictionaries from the state
		projectDicts	<- getsRef stateProject

		crushProj_withObj cid src fProj clsObj tObj projectDicts

	
crushProj_withObj cid src 
	fProj@(FProj proj _ _ _) 
	cObj tObj
	projectDicts

	-- This isn't a type constructor, hopefully something will be unified
	--	into it later. Just return without doing anything more.
	| TSum _ []			<- tObj
	= do	trace $ ppr "  * We don't have an object type, deferring.\n\n"

		-- Reactivate the class so we get called again during the next grind.
		activateClass cid
		return Nothing

	-- The object is a constructor, but there's no projection dictionary for it.
	--	This is a type error.	
	| Just (vCon, _, _)	<- takeTData tObj
	, Nothing		<- Map.lookup vCon projectDicts
	= do	trace $ ppr "  * No projections defined for this data type, error.\n\n"
		addErrors
		  [ErrorNoProjections
			{ eProj		= proj
			, eConstructor	= tObj }]
		return Nothing
	
	-- yay, we've got a projection dictionary
	| Just (vCon, _, _)	<- takeTData tObj
	, Just vsDict		<- Map.lookup vCon projectDicts
	= do	trace $ ppr "  * We've got a projection dictionary.\n"
		crushProj_withDict cid src fProj cObj tObj (snd vsDict)

	-- Functions don't have projections yet, there's no source syntax to define it.
	--	We might add them later, but for now this is a type error.
	| otherwise
	= do	trace $ ppr "  * No projections are defined for non-data types, error.\n\n"
		addErrors
		 [ErrorNoProjections
		 	{ eProj		= proj
			, eConstructor	= tObj }]
		return Nothing
		

crushProj_withDict
	:: ClassId -> TypeSource -> Fetter
	-> Class   -> Type
	-> Map Var Var 
	-> SquidM (Maybe [CTree])

crushProj_withDict
	cid src
	fProj@(FProj proj vInst tObj tBind)
	cObj tObjCon vsDict
 = do
	-- Extract the name of the projection we're looking for
 	let projName	= case proj of 
				TJField v	-> varName v
				TJFieldR v	-> "ref_" ++ varName v

	-- Try and look up the var of the implementation function.
	--	We must use field _names_, not the Var.bind for comparison because the
	--	renamer can't have known which type this field belongs to.
	let mInstV	=  liftM snd
			$  find (\(v1, _) -> varName v1 == projName)
			$  Map.toList vsDict

	trace	$ "    projection instance fn (mInstV)  = " % mInstV		% "\n"

	let result
		-- There might not be an entry in the projection dictionary for this field.
		| Nothing	<- mInstV
		= do	addErrors
		  		[ErrorFieldNotPresent
				{ eProj		= proj
				, eConstructor	= tObjCon
				, eFields	= Map.keys vsDict }]
				
			return Nothing
			
		-- We've got the name of the projection function
		| Just vImpl	<- mInstV
		= do	
			trace	$ ppr "  * Projection crushed ok\n"
			
		 	-- Lookup the type variable corresponding to it.
			Just vImplT	<- lookupSigmaVar vImpl

			-- Build the new constraints
			let qs	= 	[ CInst (TSI $ SICrushedFS cid fProj src) vInst vImplT
					, CEq   (TSI $ SICrushedFS cid fProj src) (TVar kValue $ UVar vInst) tBind ]
					 
			trace 	$ "    new constraints (qs):\n"
			 	%> "\n" %!% qs % "\n"

			-- Add an entry to the projection resolution map.
			--	This information is used in Desugar.ToCore to rewrite the projection
			--	syntax into real function calls.
			stateProjectResolve `modifyRef` Map.insert vInst vImpl

			trace	$ "  * adding entry to projection resolution table:\n"
				%> vInst % " := " % vImpl % "\n\n"

			-- We can ignore this class from now on.
			delClass cid
			
			return $ Just qs					
	result



