
-- | Export information from the final state of the type constraint solver.
module Type.Export
	( Solution(..)
	, squidExport)
where
import Type.Extract
import Type.Plug
import Shared.VarPrim
import Util
import DDC.Solve.Interface.Solution
import DDC.Solve.Error
import DDC.Solve.State
import DDC.Main.Error
import DDC.Type
import DDC.Var
import Control.DeepSeq
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import qualified Data.Sequence	as Seq

debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Export"


-- | Export useful information from the constraint solver state.
--   The constraint generator produces a list of "interesting" type variables, 
--   which correspond to the types we'll need when converting from the 
--   Desugared to Core lanugage.
--
squidExport 
	:: Set Var		-- ^ Type variables that we're interested in.
	-> SquidM Solution
	 
squidExport vsTypesPlease
 = do	trace	$ "== Export ====================================\n"
 		% "    vsTypesPlease   = " % vsTypesPlease	% "\n"
  
	mapCanon	<- exportCanonical vsTypesPlease
	mapTypes	<- exportTypes vsTypesPlease
	mapInst		<- exportInst
	mapRegionCls	<- exportRegionConstraints 
	mapProjResolve	<- getsRef stateProjectResolve

	return 	$ Solution
	 	{ solutionCanon			= strict mapCanon
		, solutionTypes			= mapTypes
		, solutionInstanceInfo		= mapInst
		, solutionRegionClasses		= strict mapRegionCls
		, solutionProjResolution	= strict mapProjResolve }


-- | Export a map giving the canonical names for each variable.
exportCanonical :: Set Var -> SquidM (Map Var Var)
exportCanonical vsTypesPlease
 = do	let vs		= Set.toList vsTypesPlease
	vsCanon		<- mapM sinkVar vs
	return		$ Map.fromList $ zip vs vsCanon
	
	
-- | Export types from the graph for all these vars.
exportTypes :: Set Var -> SquidM (Map Var Type)
exportTypes vsTypesPlease 
 = do	vts 	<- mapM (\v -> do 
			mT <- exportVarType v
			return	(v, mT))
		$ Set.toList vsTypesPlease

	return	$! deepSeq vts (Map.fromList vts)


-- | Export the type for this variable.
--	If no type is in the graph for this var then return Nothing.
exportVarType :: Var -> SquidM Type
exportVarType v
 = do 	trace	$ "*   Export.exportVarType: " % v	% "\n"
 
 	mT	<- extractType True v
	tEx	<- case mT of
			Nothing	-> panic stage $ "exporVarType: export of " % v % "failed\n"
			Just t	
			 -> do	t'	<- exportType t
			 	return t'
	return tEx


-- | Process this type to make it an exportable format.
exportType :: Type -> SquidM Type
exportType t
 = do	tPlug		<- plugClassIds Set.empty t

 	quantVars	<- liftM (Set.fromList . Map.keys)
			$  getsRef stateQuantifiedVarsKM

	let tFinal	= finaliseT quantVars True tPlug

	trace	$ "*   Export.exportType: final\n"
		% "    tPlug:\n" 	%> prettyTypeSplit tPlug	% "\n"
		% "    tFinal:\n"	%> prettyTypeSplit tPlug	% "\n"

	let kind	= kindOfType tFinal
	let tTrim
		| isClosureKind kind	= trimClosureC tFinal
		| isValueKind   kind 	= trimClosureT tFinal
		| otherwise		= tFinal
				
	trace	$ "    tTrim:\n"	%> prettyTypeSplit tTrim	% "\n\n"

	return tTrim	
 

exportMaybeType :: Maybe Type -> SquidM (Maybe Type)
exportMaybeType mt
 = case mt of
 	Nothing	-> return Nothing
	Just t 
	 -> do	t'	<- exportType t
	 	return	(Just t')

		
-- | Build a map of all the instantiations
exportInst :: SquidM (Map Var (InstanceInfo Type))
exportInst 
 = do	inst	<- getsRef stateInst
	vts	<- mapM exportInstInfo $ Map.toList inst
			
	return	$ Map.fromList vts

exportInstInfo 	:: (Var, InstanceInfo Var)
		-> SquidM (Var, InstanceInfo Type)

exportInstInfo (v, ii)
 = case ii of	
 	InstanceLambda v1 v2 mt
	 -> do	mt'		<- exportMaybeType mt
	 	return		$ (v, InstanceLambda v1 v2 mt)

	InstanceLet    v1 v2 vs t
	 -> do	ts 	<- mapM exportVarType vs

		-- HACKS: When we take the type to use for an insantiation we want the ungeneralised
		--        one, but the eq class will already have been updated with the generalied 
		--        scheme. We should really have separate classes for this, but assuming we're
		--	  only dealing with rank-1 types we can just take the scheme and chop off the foralls.
		--
		--	  See test/Typing/Closure/GetInc1 for an example that runs into this problem.

		let chopForalls tt = case tt of
					TForall b k t	-> chopForalls t
					_		-> tt

		let ts_hacked	= map chopForalls ts
		
		-- need to finalise again because quantified vars have been chopped off
 		quantVars	<- getsRef stateQuantifiedVars
		let ts_final	= map (finaliseT quantVars True) ts_hacked
	 	t'		<- exportType t

		trace 	$ "*   Export.exportInstInfo " % v % "\n"
			% "    tt = " % ii % "\n"
			% "    ts:\n" 		%> "\n" %!% ts		% "\n\n"
			% "    ts_final\n"	%> "\n" %!% ts_final	% "\n\n"
			% "    t:\n"		%> prettyTypeSplit t		% "\n\n"
			% "    t':\n"		%> prettyTypeSplit t'		% "\n\n"

	 	return		$ (v, InstanceLet v1 v2 ts_final t')
		
	InstanceLetRec 	vUse vDef Nothing
	 -> do  tDef	<- exportVarType vDef
	 	return	$ (v, InstanceLetRec vUse vDef (Just tDef))
	 

-- | Build a map of the constraints acting on each region
exportRegionConstraints 
	:: SquidM (Map Var [Var])
	
exportRegionConstraints
 	= foldClasses slurpRsConstraints Map.empty 
 	
slurpRsConstraints ccMap cls
  = do	mp	<- slurpRsConstraints1 cls
  	case mp of
	 Nothing	-> return ccMap
	 Just (v, cs)	-> return $ Map.insert v cs ccMap
	 	
slurpRsConstraints1 c
	-- got a region class		
 	| Class { classKind 	= kR
		, classFetters	= fsSrcs }	<- c
	, kR	== kRegion
	= do
		-- extract the vars for all the fetters acting on this class
	 	let vars	= Map.keys fsSrcs
		
		-- get the name of this class
		name	<- getCanonicalNameOfClass (classId c)

		-- check for errors on region constraints
		mErr		<- checkRegionError c
		addErrors (maybeToList mErr)
	
		return	$ Just (name, vars)
		
	 -- not a region class
	| otherwise
	= 	return	$ Nothing


-- | Check for constraint errors on this region.
checkRegionError :: Class -> SquidM (Maybe Error)
checkRegionError cls@Class { classFetters = fsSrcs }

	-- found a mutability conflict.
	| Just srcsConst	<- Map.lookup primConst   fsSrcs
	, Just srcsMutable	<- Map.lookup primMutable fsSrcs
	= let	srcConst   Seq.:< _	= Seq.viewl srcsConst
		srcMutable Seq.:< _	= Seq.viewl srcsMutable
		t			= TVar (classKind cls) $ UClass (classId cls)
		
	  in	return 	$ Just $ ErrorRegionConstraint
			{ eFetter1		= FConstraint primConst [t]
			, eFetterSource1	= srcConst
			, eFetter2		= FConstraint primMutable [t]
			, eFetterSource2	= srcMutable }
	
	-- found a direct/lazy conflict.
	| Just srcsDirect	<- Map.lookup primDirect fsSrcs
	, Just srcsLazy		<- Map.lookup primLazy   fsSrcs
	= let	srcDirect Seq.:< _	= Seq.viewl srcsDirect
		srcLazy   Seq.:< _	= Seq.viewl srcsLazy
		t			= TVar (classKind cls) $ UClass (classId cls)
	
	  in	return $ Just $ ErrorRegionConstraint
			{ eFetter1		= FConstraint primDirect [t]
			, eFetterSource1	= srcDirect
			, eFetter2		= FConstraint primLazy [t]
			, eFetterSource2	= srcLazy }
		
	-- no problems.
	| otherwise
	= return $ Nothing

