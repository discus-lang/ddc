
module Type.Solve.Finalise
	(solveFinalise)
where
import Type.Solve.Generalise
import Type.Check.Main
import Type.Check.Instances
import Type.Check.SchemeDanger
import DDC.Solve.Location
import DDC.Solve.State
import Constraint.Exp
import Data.IORef

import Util
import qualified Data.Set	as Set

-----
debug	= True
trace s	= when debug $ traceM s


-----
solveFinalise 
	:: ([CTree] -> SquidM ())	-- the solveCs function from Type.Solve
	-> Bool 			-- whether to require the main function to have
					--	the type () -> ()
	-> SquidM ()
	
solveFinalise solveCs blessMain
 = do
	-- Generalise left over types.
	--	Types are only generalised before instantiations. If a function has been defined
	--	but not instantiated here (common for libraries) then we'll need to perform the 
	--	generalisation now so we can export its type scheme.
	--
	sGenSusp		<- getsRef stateGenSusp
	let sGenLeftover 	= Set.toList sGenSusp

	trace	$ "\n== Finalise ====================================================================\n"
		% "     sGenLeftover   = " % sGenLeftover % "\n"
	
	solveCs [CGrind]

	-- If grind adds errors to the state then don't do the generalisations.
	errs	<- gotErrors
	when (not errs)
	 $ do 	
	 	mapM_ (solveGeneralise (TSI $ SIGenFinal)) $ sGenLeftover

		-- When generalised schemes are added back to the graph we can end up with (var = ctor)
		--	constraints in class queues which need to be pushed into the graph by another grind.
		--
		solveCs [CGrind]
		return ()

		
	trace $ ppr "\n=== solve: post solve checks.\n"
	
	-- If the main function was defined, then check it has an appropriate type.
	errors_checkMain	<- gets stateErrors
	when (null errors_checkMain && blessMain)
		checkMain

	-- Check there is an instance for each type class constraint left in the graph.
	errors_checkInstances	<- gets stateErrors
	when (null errors_checkInstances)
		checkInstances

	-- Check 
	errors_checkSchemes	<- gets stateErrors
	when (null errors_checkSchemes)
		checkSchemes

	-- Report how large the graph was
	graph		<- getsRef stateGraph
	size		<- liftIO $ readIORef (graphClassIdGen graph)
	trace	$ "=== Final graph size: " % size % "\n"

	-- Report whether there were any errors
	errors	<- gets stateErrors
	trace   $ "=== Errors: " % errors % "\n"

		
	return ()
