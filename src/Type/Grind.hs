-- | 

module Type.Grind
	( solveGrind )

where

-----
import Util

import qualified Data.Set	as Set
import Data.Set			(Set)

import qualified Data.Map	as Map
import Data.Map			(Map)

import qualified Shared.VarBind	as Var
import Shared.Error

import Type.Exp
import Type.Error
import Type.Util

import Type.Class
import Type.State

import Type.Crush.Unify
import Type.Crush.Fetter
import Type.Crush.Shape
import Type.Crush.Proj
import Type.Crush.Effects
import Type.Crush.Sum

-----
debug	= True
trace s	= when debug $ traceM s
stage	= "Type.Squid.Grind"


-- | Perform unification, resolve projections and grind out any available effects
--	or fetters in the graph.
solveGrind :: SquidM ()
solveGrind
 = do
	-- Grab lists of interesting equivalence classes from the register.
	register		<- gets stateRegister

	let getReg bind		
		= return $ Set.toList $ (\(Just x) -> x) $ Map.lookup bind register

	regEReadH	<- getReg Var.EReadH
	regEReadT	<- getReg Var.EReadT
	regEWriteT	<- getReg Var.EWriteT

	regFLazyH	<- getReg Var.FLazyH
	regFMutableT	<- getReg Var.FMutableT
	regFConstT	<- getReg Var.FConstT


	-- debug
	trace	$ "\n"
		% "=============================================================\n"
		% "=== Grind.solveGrind\n"
		% "    regEReadT    = " % regEReadT	% "\n"
		% "    regEReadH    = " % regEReadH	% "\n"
		% "    regFLazyH    = " % regFLazyH	% "\n"
		% "    regFMutableT = " % regFMutableT	% "\n"
		% "    regFConstT   = " % regFConstT	% "\n"
		% "\n\n"

	-- Run the unifier.
	trace	$ prettyp "*   Grind.solveGrind, unifying.\n"
	solveUnify


	-- Now that the graph is unified, we can try and crush out some of the simpler compound
	--	effects and fetters. Crushing these constructors will not add any more constraints
	--	to nodes in the graph, so there is no need to interleave it with unification.

	-- Crush out EReadTs
	trace	$ prettyp "*   Grind.solveGrind, crushing EReadHs, EReadTs, EWriteTs\n"
	mapM_ crushEffectC (regEReadH ++ regEReadT ++ regEWriteT)

	-- Crush out FLazyHs, FMutableTs
--	trace	$ prettyp "*   Grind.solveGrind, crushing FLazyHs, FMutableTs\n"
--	mapM_ crushFetterC (regFLazyH ++ regFMutableT)
	
	-- all done
	trace	$ "\n"
		% "=== Grind.solveGrind done\n"
		% "=============================================================\n"
		% "\n\n"

	return ()


-- Unify some classes in the graph.
--	The crushing of Shape and projection fetters is interleaved with batches unification
--	because the crushing can add more constraints to the graph.
--
solveUnify ::	SquidM ()
solveUnify 	
 = do	-- get the list of nodes which have constraints waiting to be unified.
 	queued		<- liftM Set.toList $ clearActive 		

	-- get classes waiting to be projected
	regProj		<- getRegProj		

	errors		<- gets stateErrors
	solveUnifySpin queued regProj errors

solveUnifySpin queued regProj errors

	-- If there are errors in the solver state then bail out.
	| not $ isNil errors
	= return ()
	
	-- If no nodes need to be unified and there are no projections left
	--	in the graph then we're done.
	| []	<- queued
	, []	<- regProj
	= return ()
	
	-- Otherwise, try to unify or crush something.
	| otherwise
	= solveUnifyWork queued regProj errors

solveUnifyWork queued regProj errors
 = do	trace	$ "*   Grid.solveUnifyWork\n"
		% "    queued      = " % queued		% "\n"

  	-- Try to unify some of the queued classes.
  	mapM_ crushUnifyClass queued

	-- Try to crush out some of the Shape fetters.
	regShapes	<- getRegShapes
	trace	$ "    regShapes   = " % regShapes	% "\n"
	mapM crushShape regShapes

{-
	-- Try to crush out some of the FieldIs fetters.
	crushedSomeProjs
		<- liftM or 
		$  mapM crushProjClassT regProj

	regProj'	<- getRegProj
-}
	-- Check to see if we've made progress with the graph.
	--	If we haven't unified anything, and haven't crushed out any of the
	--	FFieldIs fetters then we're stalled and have an ambiguous projection
	--	somewhere.
	-- 
--	let progress	= False

	let progress
		=  (not $ isNil queued)
--		|| crushedSomeProjs

	-- debug
{-	trace	$ "*   Grind.solveUnify\n"
		% "    queued      = " % queued		% "\n"
--		% "    regProj     = " % regProj	% "\n"
--		% "    regProj'    = " % regProj'	% "\n"
		% "    progress    = " % progress	% "\n"
		% "\n"
-}
	trace	$ prettyp "\n"
	solveUnify

{-	if progress
		then	solveUnify
		else	errorProjection regProj'
-}


-- | Get the list of classe which contain projection fetters.
getRegProj :: SquidM [ClassId]			  	
getRegProj
 = do	register	<- gets stateRegister
	let regProj
		= Set.toList
		$ (\(Just x) -> x)
		$ Map.lookup Var.FProj register

	return regProj


-- | Get the list of classes which contain shape fetters.
getRegShapes :: SquidM [ClassId]
getRegShapes
 = do	register	<- gets stateRegister
	let regShapes
		= Set.toList
		$ (\(Just x) -> x)
		$ Map.lookup (Var.FShape 0) register

	return regShapes


-- | Add an ambiguous projection error to the solver state.
errorProjection :: [ClassId] -> SquidM ()
errorProjection (cid:_)
 = do
	-- Lookup one of the classes and extract the offending FielsIs fetters.
	--
 	Just c	<- lookupClass cid

	let (TFetter (FProj f _ _ _ _ _))
		= classType c
	
	addErrors 
		[ErrorAmbiguousProjection
		{ eProj		= f }]

	return ()
	
