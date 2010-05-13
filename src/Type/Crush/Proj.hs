
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
	(crushProjClassT)
where
import Type.Location
import Type.Exp
import Type.Builtin
import Type.Util
import Type.Error
import Type.Class
import Type.State
import Constraint.Exp
import Util
import DDC.Main.Error
import DDC.Solve.Trace
import DDC.Var
import qualified Data.Map	as Map

-----
stage	= "Type.Crush.Proj"
debug	= False
trace s	= when debug $ traceM s


-- | Crush out projection constraints from an equivalence class.
--	This function returns new constraints containing the type
--	of the projection instance function.
--
crushProjClassT 
	:: ClassId 			-- class containing constraints
	-> SquidM (Maybe [CTree])	-- new constraints to add to the graph

crushProjClassT cidT
 = do	 
 	-- Check for errors already in the graph.
	-- 	The grinder shouldn't be calling us if there are already errors.
 	errs	<- gets stateErrors
	when (not $ isNil errs)
	 $ panic stage	$ "crushProjClassT: state has errors\n"
	 		% errs
  
	-- Grab the projection constraint from the graph node.
 	Just cProj@(ClassFetter 
			{ classFetter = fProj 
			, classSource	= src })
				<- lookupClass cidT

	trace	$ "*   Proj.crushProjClassT\n"
		% "    classId of fetter  (cidT)        = " % cidT		% "\n"
		% "    fetter                           = " % fProj		% "\n"

	let FProj _ _ (TClass _ cidObj) _	
			= fProj
	
	-- The "object type" is the type we're using to determine which projection
	--	instance function to use. Starting from the root of this type, 
	--	walk down its left spine to see if there is a tycon at the front.
	--	If there is, then return the classId of the node containing this 
	--	tycon.
	mClsObj		<- classDownLeftSpine cidObj
	
	case mClsObj of
	 -- The unifier needs to unify the nodes in this class before we know
	 --	what the object type will be.
	 Just cObj@(Class { classType = Nothing })
	  -> do	trace $ ppr "    -- classType is Nothing. Object class not unified yet, deferring.\n\n"
	 	return Nothing

	 -- Ok, we've got an object type, carry on.
	 Just cObj@(Class { classType = Just nObj })
	  -> do	Just tObj	<- lookupTypeOfCidAsSquid cidObj
		crushProj_withObj cidT src fProj cObj tObj
	
	
crushProj_withObj cidT src fProj cObj tObj
 = do
	trace	$ "    object type (cObj)               = " % classType cObj	% "\n"

 	let FProj proj _ (TClass _ _) _	
			= fProj

	-- Grab the map of projection dictionaries from the state
	projectDicts	<- gets stateProject

	-- If the object type has resolved to a type constructor we should
	--	be able to get the projection dictionary for it.
	let result
		-- This isn't a type constructor, hopefully something will be unified
		--	into it later. Just return without doing anything more.
		| TSum _ []			<- tObj
		= do	trace $ ppr "    -- We don't have an object type, deferring.\n\n"

			-- Reactivate the class so we get called again during the next grind.
			--	This hasn't been done automatically because we haven't
			--	modified it yet.
			activateClass cidT

			return Nothing

		-- The object is a constructor, but there's no projection dictionary for it.
		--	This is a type error.	
		| Just (vCon, _, _)	<- takeTData tObj
		, Nothing		<- Map.lookup vCon projectDicts
		= do	trace $ ppr "    -- No projections defined for this data type, error.\n\n"
			addErrors
			  [ErrorNoProjections
				{ eProj		= proj
				, eConstructor	= tObj }]
			return Nothing
	
		-- yay, we've got a projection dictionary
		| Just (vCon, _, _)	<- takeTData tObj
		, Just vsDict		<- Map.lookup vCon projectDicts
		= do	trace $ ppr "    -- We've got a projection dictionary.\n"
			crushProj_withDict cidT src fProj cObj tObj (snd vsDict)

		-- Functions don't have projections yet, there's no source syntax to define it.
		--	We might add them later, but for now this is a type error.
		| otherwise
		= do	trace $ ppr $ " -- No projections are defined for non-data types, error.\n\n"
			addErrors
			 [ErrorNoProjections
			 	{ eProj		= proj
				, eConstructor	= tObj }]
			return Nothing
		
	result


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
			trace	$ ppr "    -- Projection crushed ok\n"
			
		 	-- Lookup the type variable corresponding to it.
			Just vImplT	<- lookupSigmaVar vImpl

			-- Build the new constraints
			let qs	= 	[ CInst (TSI $ SICrushedFS cid fProj src) vInst vImplT
					, CEq   (TSI $ SICrushedFS cid fProj src) (TVar kValue vInst) tBind ]
					 
			trace 	$ "    new constraints (qs):\n"
			 	%> "\n" %!% qs % "\n"

			-- Add an entry to the projection resolution map.
			--	This information is used in Desugar.ToCore to rewrite the projection
			--	syntax into real function calls.
			modify $ \s -> s { stateProjectResolve 
						= Map.insert vInst vImpl (stateProjectResolve s) }

			trace	$ "    adding entry to projection resolution table:\n"
				%> vInst % " := " % vImpl % "\n\n"

			-- We can ignore this class from now on.
			delClass cid
			
			return $ Just qs					
	result


-- | Walk down the left spine of this type to find the type in the bottom 
--	left node (if there is one)
--
--	For example, if the graph holds a type like:
--	   TApp (TApp (TCon tc) t1) t2
--	
--	Then starting from the cid of the outermost TApp, we'll walk down 
--	the left spine until we find (TCon tc), and return that.
--
--	If the node at the bottom of the spine hasn't been unified, then
--	It'll be a Nothing, so return that instead.
--
classDownLeftSpine :: ClassId -> SquidM (Maybe Class)
classDownLeftSpine cid
 = do	Just cls	<- lookupClass cid
	case classType cls of
	 Just (NApp cid1 _)	
		-> classDownLeftSpine cid1
	 mType	-> return $ Just cls

