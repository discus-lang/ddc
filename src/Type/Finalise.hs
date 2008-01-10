
module Type.Finalise
	() -- solveFinalise )

where

import Util
import Data.Array.IO

import qualified Data.Map	as Map
import Data.Map			(Map)

import Type.Exp
import Type.Pretty
import Type.Util

import Type.Base
import Type.State
import Type.Class

import Type.Scheme

import Type.Error


-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Finalise"

-----
{-
solveFinalise :: SquidM ()
solveFinalise 
 = return ()
 -}
{-
 do	
 	trace	$ "\n\n"
		% "========================================================================================== FINALISE\n"

	-- Do a final grind to make sure that everything needing to be 
	--	unified is unified.
	--
 	solveGrind

	-- If there were errors, then bail out.
	--
	errs	<- gets stateErrors
	case errs of
	 []	-> solveFinalise2
	 _	-> return ()

-}



 
{-
 = do

	 
solveFinalise2
 = do	
 	trace	$ prettyp $ "=== Checking schemes.\n"
 	
	schemeGen	<- gets stateGen
	schemes'	<- mapM (\(v, (tScheme, envCids)) -> checkScheme v tScheme envCids)
				$ Map.toList schemeGen

	modify (\s -> s {
		stateSchemeInferred
			= Map.fromList schemes' })

	trace	$ prettyp "\n\n"
	
	solveFinalise3

solveFinalise3
 = do
	school		<- gets stateClass	

	-- BUGS: this is prob wrong.. does nil classes
	classes		<- liftIO (getElems $ schoolClass school)
	let cids	= [ classId c | c@Class{}	<- classes]

	let totalClasses
		= schoolClassIdGen school

	trace	$ "=== Finalising suspended schemes\n"
		% "    classes alloced = " % totalClasses	% "\n"
		% "    classes active  = " % length cids	% "\n"
		% "\n"

	-- Mash all the variable names together so that we've got a
	--	unique name for each class. This just adds the variable
	--	substitutions, we'll still need to update the exported types
	--	to make sure the names are current.
	--
	mapM_ mashClassT cids
	
	-- Debugging
		
	-- All done.
	return ()


-----
checkScheme varT tScheme cidsEnv_
 = do	
	-- Classes might have changed since we generalised the scheme the first time,
	--	so sink them again now
	cidsEnv		<- mapM sinkClassId cidsEnv_

	-- Regeneralise the scheme now that all constraints are in the graph.
	tGraph		<- extractType    varT cidsEnv
	tRegen		<- generaliseType varT tGraph cidsEnv

	-- Compare how each scheme is quantified.
	let schemeShape	= chopToShape tScheme
	let regenShape	= chopToShape tRegen
	let bits	= unifyT schemeShape regenShape
	let conflicts	= filter isQuantConflict bits
		
	trace	$ "*   Solve.checkScheme " % varT 	% "\n"
		% "    cidsEnv = " % cidsEnv		% "\n"
		% "    tScheme\n"
		%> prettyTS tScheme
		% "\n\n"
		% "    tRegen\n"
		%> prettyTS tRegen
		% "\n\n"
		% "    bits      = " % bits		% "\n"
		% "\n"
		% "    conflicts = " % conflicts	% "\n"
		% "\n\n"

	case conflicts of
	 [] -> do 
		-- No conflicts, return the regeneralised scheme.
		--	We return the regeneralised scheme here, because we can still
		--	pick up extra Mutability constraints which aren't causing
		--	quantification problems.
		--
	 	return (varT, tRegen)

	 _ -> do
		-- If the schemes are quantified differently then we're screwed.
	 	addErrors 
			[ErrorLateConstraint
			{ eScheme	= (varT, tScheme)
			, eRegen	= tRegen }]
			
		return (varT, tRegen)


isQuantConflict tt
 = case tt of
 	(TClass{}, 		TVar{})			-> True
	(TVar{},   		TClass{})		-> True

	(TRegion RClass{},	TRegion RVar{})		-> True
	(TRegion RVar{},	TRegion RClass{})	-> True
	
	(TEffect EClass{},	TEffect EVar{})		-> True
	(TEffect EVar{},	TEffect EClass{})	-> True
	
	(TClosure CClass{},	TClosure CVar{})	-> True
	(TClosure CVar{},	TClosure CClass{})	-> True
	
	(_, _)						-> False
-}
