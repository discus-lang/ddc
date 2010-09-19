{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
{-# OPTIONS -fno-warn-incomplete-patterns -fno-warn-unused-binds #-}

module DDC.Solve.Crush.Shape
	(crushShapeInClass)
where
import Type.Feed
import Shared.VarPrim
import DDC.Solve.Walk
import DDC.Solve.Location
import DDC.Solve.Crush.Unify
import DDC.Solve.State
import DDC.Type.Data
import DDC.Type.Data.Pretty		()
import DDC.Type
import DDC.Main.Error
import DDC.Main.Pretty
import Util
import qualified Data.Map		as Map

debug	= False
trace s	= when debug $ traceM s
stage	= "DDC.Solve.Crush.Shape"

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
	let cidsMerge	= map (\(TVar _ (UClass cid)) -> cid) shapeTs

	trace	$ vcat
		[ "-- Crush.shape " 		% cidShape
		, "   fetter          = "	% fShape
		, "   cidsMerge       = "	% cidsMerge
		, blank ]

 	-- Ensure that all the classes to be merged are unified.
	-- We need this because resolving shape constraints can add more nodes
	-- to an equivalence class. So if the grinder is just calling crushShapeInClass
	-- on a set of cids the unifier won't get to run otherwise.
 	mapM crushUnifyInClass cidsMerge
 
	-- diagnose the constrained classes to see if we have any templates.
	diagsMerge		<- mapM diagShapeConstrainedCid cidsMerge
	let mDiagTemplate	= takeHead $ filter isDiagTemplate diagsMerge

	trace	$ vcat
		[ ppr "-- Crush.shape"  %% cidShape %% "(diag)"
		, "   diags:\n"         %> vcat diagsMerge
		, "   mDiagTemplate   = " % mDiagTemplate
		, blank ]
	
	-- the source to use for the new constraints.
	let srcCrushed	= TSI $ SICrushedFS cidShape fShape srcShape
	
	crushShapeWithTemplate cidShape srcCrushed diagsMerge mDiagTemplate	


-- None of the nodes contain data constructors, so there's no template to work from.
-- However, something might be unified in later, so activiate ourselves
-- so the grinder calls us again on the next pass.
crushShapeWithTemplate cidShape _srcCrushed _diags Nothing
 = do	trace	$ vcat
		[ "-- Crush.shape" %% cidShape %% "(template)"
		, ppr "   -- no templates, reactivating"
		, blank]

	activateClass cidShape
	return False


-- We've got a template, so we need to constrain the other classes so they
-- match the template.
crushShapeWithTemplate cidShape srcCrushed diags (Just diagTemplate)
 = do	trace	$ vcat
		[ "-- Crush.shape" %% cidShape %% "(template)"
		, "   diagTemplate    = "       % diagTemplate
		, blank]

	let DiagTemplate cidTemplate nTemplateCtor cidsTemplateLeaves
		= diagTemplate
		
	-- Get the leaf classes of the template.
	--   The leaf classes are the ones that hold the type constructor and its arguments.
	--   For example, if the template type looks like this:
	--  	C %1 *2 !3
	--   Then the leaf classes are the one that holds the constructor C, 
	--   along with the three argument classes %1 *2 !3. 
	--   We don't care about the classes that hold intermediate applications, like for (C %1).
	Just clsTemplateLeaves
		<- liftM sequence 
		$  mapM lookupClass cidsTemplateLeaves

	-- As we've already chosen the template we're working with, set the diags
	-- of the other Shape constrained classes to be receivers of this template.
	let diags_recv	
		= map diagMakeReceiver
		$ filter (\d -> cidOfDiag d /= cidOfDiag diagTemplate) diags

	-- Instantiate the template for each of the Shape constrained classes.
	(cidsNewSpinesRoot, cidssNewSpinesLeaves)
		<- liftM unzip 
		$  replicateM (length diags_recv)
		$  createSpine srcCrushed kValue (reverse clsTemplateLeaves)

	-- The new spines are fresh and currently have no relationship with 
	-- the classes being constrained, so we merge their root classes 
	-- with the ones actually being constrained by the Shape.
	cidsNewSpinesRoot_merged
		<- zipWithM (\cid1 cid2 -> mergeClasses [cid1, cid2])
			(map cidOfDiag diags_recv)
			cidsNewSpinesRoot
	
	-- Match up leaves of the template and the ones we've just made.
	let cidssMergeLeaves
		= transpose $ (cidssNewSpinesLeaves ++ [cidsTemplateLeaves])
	
	-- The first one of these leaves should contain the type constructor
	-- and createSpines will have already added it to the fresh ones, 
	-- so we don't have to worry about it. How we constrain the arguments
	-- of the constructor depends on what kind they are, and whether
	-- they correspond to material positions of the data type or not.
	let (_ : cidssMergeArgs) = cidssMergeLeaves
		
	trace	$ vcat
		[ "-- Crush.shape" %% cidShape %% "(template) "
		, "   diags_recv:\n" 			%> vcat diags_recv
		, "   cidsNewSpinesRoot:          "	%> cidsNewSpinesRoot
		, "   cidsNewSpinesRoot_merged:   "	%> cidsNewSpinesRoot_merged
		, "   cidssNewSpinesLeaves:       "	%> cidssNewSpinesLeaves
		, "   cidssMergeLeaves:           "	%> cidssMergeLeaves
		, "   cidssMergeArgs:             "	%> cidssMergeArgs
		, blank]

	-- At this point we've made the top-level of the constrained types
	-- the same, we now have to constrain the arguments appropriately.

	-- We need to now what args are material, so grab the data type
	-- definition for the template type constructor.
	dataDefs <- liftM squidEnvDataDefs $ gets stateEnv
	
	-- Lookup the materialities for the type
	let NCon tc	  	= nTemplateCtor
	let TyConData v _	= tc
	let Just dataDef	= Map.lookup v dataDefs
	let Just materiality	= paramMaterialityOfDataDef dataDef
	let kinds		= map snd $ dataDefParams dataDef
	
	-- Add constraints depending on what materiality the parameters are.
	mapM (mergeMaterial srcCrushed) 
		$ zip3 materiality kinds cidssMergeArgs

	trace	$ vcat
		[ "   dataDef:\n"		%> dataDef
		, "   materiality:       "	% materiality ]
	
	-- We've discharged the constraint by modifying the graph appropriately, 
	-- so we don't need the original fetter any more.
	delMultiFetter cidShape
	return True


mergeMaterial :: TypeSource -> (Materiality, Kind, [ClassId]) -> SquidM ()
mergeMaterial srcCrushed (mm, kind, cids)
	| elem mm [MaterialAbsent, MaterialNot, MaterialMixed]
	= do	mergeClasses cids
		return ()

	-- strongly material.
	| isRegionKind kind
	= return ()
	
	| isValueKind kind
	= addShapeFetter srcCrushed cids
	
	| otherwise
	= panic stage $ "mergeMaterial: no match"
	
	
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


-- Spine ------------------------------------------------------------------------------------------
--   Create a new spine of type applications based on the kinds of a list of classes.
--   The spine is built in reverse order, so in the following example the first
--   class in the list is an effect class, and the last contains the constructor
--   we put in *1.
--
--   The last class must contain a constructor, else panic.
--
--  @
--            / 
--          app 
--         /   \\
--       app    !4
--      /   \\          
--    app    *3 
--   /   \\
--  *1    %2
-- @
-- 
createSpine 
	:: TypeSource	-- ^ Source to use for the new classes.
	-> Kind		-- ^ Kind of the result of the constructor.
	-> [Class]	-- ^ Leaf classes to use to get the kinds of the new classes.
	-> SquidM (ClassId , [ClassId])
			-- ^ Class holding the top level application, and the list
			--   of leaf classes, [*1, %2, *3, !4] in this example.

createSpine src kResult clss

	| [cls]	<- clss
	, Class { classUnified = Just nCtor@(NCon _) } <- cls
	= do	cidHere'	<- allocClass (classKind cls) src
		addNodeToClass cidHere' (classKind cls) src nCtor
		return (cidHere', [cidHere'])
	
	| clsRight : clssRest	<- clss
	= do	let kRight	= classKind clsRight
		cidRight'	<- allocClass kRight src
	
		let kLeft	= KFun kRight kResult
		(cidLeft', cidsArgLeft)
				<- createSpine src kLeft clssRest

		let nHere	= NApp cidLeft' cidRight'
		cidHere'	<- allocClass kResult src
		addNodeToClass cidHere' kResult src nHere
		return (cidHere', cidsArgLeft ++ [cidRight'])


-- Diag -------------------------------------------------------------------------------------------
data Diag
	= DiagTemplate ClassId Node [ClassId]
	| DiagReceiver ClassId 
	| DiagNotReady ClassId
	deriving (Show, Eq)


-- | Get the classId of a diag
cidOfDiag :: Diag -> ClassId
cidOfDiag diag
 = case diag of
	DiagTemplate cid _ _	-> cid
	DiagReceiver cid 	-> cid
	DiagNotReady cid	-> cid
	

-- | Check if a diag is a template.
isDiagTemplate :: Diag -> Bool
isDiagTemplate diag
 = case diag of
	DiagTemplate{}	-> True
	_		-> False

instance Pretty Diag PMode where
 ppr diag
  = case diag of
	DiagTemplate cid node cidsApps 	-> "DiagTemplate" %% cid %% node %% cidsApps
	DiagReceiver cid                -> "DiagReceiver" %% cid
	DiagNotReady cid		-> "DiagNotReady" %% cid


-- Look at the types being constrained.
-- We get back a list of cids that might look something like this:
--        [[*1, %2, *3], [*6]]
-- 
-- When there are more than one element in the inner list, the first one
-- is likely to hold a data construcor. For example:
--          *1 = List, 
--          %2 is it's primary region
--          *3 = (Int %8)
-- 
-- For *6, this is probably an unconstrained class that doesn't have any 
-- constructors in it yet.
--
diagShapeConstrainedCid :: ClassId -> SquidM Diag
diagShapeConstrainedCid cid
 = do	mCidsApps	<- takeAppsDownLeftSpine cid

	whenMaybeM mCidsApps 
	  (return $ DiagNotReady cid) 
	  (\cidsApps -> do
		let Just cidHead =  takeHead cidsApps
		Just clsHead     <- lookupClass cidHead
		
		case classUnified clsHead of
		 Just n@(NCon (TyConData _ _))
				-> return $ DiagTemplate cid n cidsApps
		 _		-> return $ DiagReceiver cid)


-- | If this diag does not refer to the given cid then turn it into a receiver.
diagMakeReceiver :: Diag -> Diag
diagMakeReceiver diag
	= DiagReceiver (cidOfDiag diag)

