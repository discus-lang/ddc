
-- | Export information from the final state of the type constraint solver.
--
module Type.Export
	( squidExport )

where

import Type.Exp

import Type.Pretty
import Type.Error
import Type.Base
import Type.Class
import Type.State
import Type.Extract
import Type.Plug
import Type.Util
import Type.Plate.Trans
import Type.Plate.FreeVars
import Type.Context
import Type.Location

import Data.Array.MArray
import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Shared.Var	as Var
import Shared.Var		(Var, NameSpace(..))
import Shared.VarPrim

import Shared.Error
import Util

import qualified Debug.Trace

-----
debug	= False
trace s	= when debug $ traceM s
stage	= "Type.Export"

-- | Export some stuff from the constraint solver state.
squidExport 
	:: Set Var		-- ^ vars of the bindings we want types for.
	-> SquidM 
		( Map Var Type				-- type schemes.
		, Map Var (InstanceInfo Type Type)	-- how each instantiation was done.
		, Map Var (Kind, Maybe Type)		-- which vars were quantified (with optional :> bound)
		, Map Var [Var])			-- the constraints acting on each region.

squidExport 
	vsTypesPlease
 = do	trace	$ "== Export ====================================\n"
 		% "    vsTypesPlease   = " % vsTypesPlease	% "\n"
  
 	-- Export types for the requested vars.
	types		<- exportTypes vsTypesPlease

	-- Export the instantiation table.
	inst		<- exportInst

	-- The port table was already plugged by Scheme.generaliseType
	quantVars	<- gets stateQuantifiedVars

	-- Build a map of the constraints acting on each region
	vsRegionClasses	<- exportRegionConstraints 

	return 	( types
		, inst 
		, quantVars
		, vsRegionClasses)


-- Export types from the graph for all these vars.
exportTypes 
	:: Set Var 	-- export types for these vars
	-> SquidM (Map Var Type)

exportTypes vsTypesPlease 
 = do	
 	-- these are the type that were asked for by the slurper.
 	let vsPlease	=  Set.toList vsTypesPlease

	-- export types for all the vars asked for
	vts 	<- mapM (\v -> do 
			mT <- exportVarType v
			return	(v, mT))
		$ vsPlease

	return	$ Map.fromList vts


-- | Export the type for this variable.
--	If no type is in the graph for this var then return Nothing.
--
exportVarType :: Var -> SquidM Type
exportVarType v
 = do 	trace	$ "*   Export.exportVarType: " % v	% "\n"
 
 	mT	<- extractType True v
	tEx	<- case mT of
			Nothing	-> 
				panic stage $ "exporVarType: export of " % v % "failed\n"

			Just t	
			 -> do	t'	<- exportType t
			 	return t'
	return tEx


-- | Process this type to make it an exportable format.
--	plug classids.
--	trim closures.
--	bottom-out non-port effect/closure variables.
--
exportType :: Type -> SquidM Type
exportType t
 = do	tPlug		<- plugClassIds [] t

	quantVars	<- gets stateQuantifiedVars
	let tFinal	= finaliseT quantVars True tPlug

	trace	$ "*   Export.exportType: final\n"
		% "    tPlug:\n" 	%> prettyTS tPlug	% "\n"
		% "    tFinal:\n"	%> prettyTS tPlug	% "\n"

	let tTrim	
		= case kindOfType tFinal of
			Just kC | kC == kClosure	-> trimClosureC Set.empty Set.empty tFinal
			Just kV | kV == kValue		-> trimClosureT Set.empty Set.empty tFinal
			Just _				-> tFinal
				
	trace	$ "    tTrim:\n"	%> prettyTS tTrim	% "\n\n"
	return tTrim		
 


exportMaybeType :: Maybe Type -> SquidM (Maybe Type)
exportMaybeType mt
 = case mt of
 	Nothing	-> return Nothing
	Just t 
	 -> do	t'	<- exportType t
	 	return	$ Just t'

		
-----
-- | Build a map of all the instantiations
--
exportInst :: SquidM (Map Var (InstanceInfo Type Type))
exportInst 
 = do	inst	<- gets stateInst
	vts	<- mapM exportInstInfo
		$  Map.toList inst
			
	return	$ Map.fromList vts

exportInstInfo 	:: (Var, InstanceInfo Var Type)
		-> SquidM (Var, InstanceInfo Type Type)

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
		quantVars	<- gets stateQuantifiedVars
		let ts_final	= map (finaliseT quantVars True) ts_hacked
	 	t'		<- exportType t

		trace 	$ "*   Export.exportInstInfo " % v % "\n"
			% "    tt = " % ii % "\n"
			% "    ts:\n" 		%> "\n" %!% ts		% "\n\n"
			% "    ts_final\n"	%> "\n" %!% ts_final	% "\n\n"
			% "    t:\n"		%> prettyTS t		% "\n\n"
			% "    t':\n"		%> prettyTS t'		% "\n\n"

	 	return		$ (v, InstanceLet v1 v2 ts_final t')
		
	InstanceLetRec 	vUse vDef Nothing
	 -> do  tDef	<- exportVarType vDef
	 	return	$ (v, InstanceLetRec vUse vDef (Just tDef))
	 

--------------------------------------------------------------------------------

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
 	| Class { classKind 		= kR
		, classFetterSources	= fs_src }	<- c
	, kR	== kRegion
	= do
		-- extract the vars for all the fetters acting on this class
	 	let vars	= map (\(FConstraint vC _, _) -> vC) fs_src
 		
		-- get the name of this class
		name	<- makeClassName (classId c)

		-- check for errors on region constraints
		mErr		<- checkRegionError c
		addErrors (maybeToList mErr)
	
		return	$ Just (name, vars)
		
	 -- not a region class
	| otherwise
	= 	return	$ Nothing


-- | Check for constraint errors on this region.
checkRegionError :: Class -> SquidM (Maybe Error)
checkRegionError c@Class 
	{ classFetterSources 	= fs_src }

	-- found a mutability conflict.
	| Just (fConst,   srcConst)	<- find (isFConstraintNode primConst)   fs_src
	, Just (fMutable, srcMutable)	<- find (isFConstraintNode primMutable) fs_src
	= do	
		return 	$ Just $ ErrorRegionConstraint
			{ eFetter1		= fConst
			, eFetterSource1	= srcConst
			, eFetter2		= fMutable
			, eFetterSource2	= srcMutable }
	
	-- found a direct/lazy conflict.
	| Just (fDirect, tsDirect)	<- find (isFConstraintNode primDirect)  fs_src
	, Just (fLazy, tsLazy)		<- find (isFConstraintNode primLazy)    fs_src
	= return $ Just $ ErrorRegionConstraint
			{ eFetter1		= fDirect
			, eFetterSource1	= tsDirect
			, eFetter2		= fLazy
			, eFetterSource2	= tsLazy }
		
	-- no problems.
	| otherwise
	= return $ Nothing


isFConstraintNode v1 (t, n)
	| FConstraint v2 _	<- t
	= v1 == v2
	
	| otherwise
	= False


