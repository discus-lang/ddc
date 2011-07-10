
module DDC.War.Controller
	( JobResult(..)
	, ChanResult
	, controller)
where
import DDC.War.Job
import DDC.War.Result
import DDC.War.Config
import DDC.War.Pretty
import BuildBox
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import System.IO
import System.Directory

data JobResult
 	= JobResult 
	{ _jobResultChainIx	:: Int
	, _jobResultJobIx	:: Int
	, _jobResultJob		:: Job
	, _jobResultResults	:: [Result] }

type ChanResult
	= TChan JobResult
	

controller 
	:: Config
	-> Gang
	-> Int		-- ^ total number of chains
	-> ChanResult	-- ^ channel to receive results from
	-> IO ()

controller config gang chainsTotal chanResult
 = go_start
 where	
	-- See if there is an input on the console.
	go_start 
	 =  hReady stdin >>= \gotInput
	 -> if gotInput
		then go_input
		else go_checkResult
	
	go_input
	 = do	putStrLn "Interrupt. Waiting for running jobs (CTRL-C kills)..."
	 	flushGang gang
			
	go_checkResult
	 =  (atomically $ isEmptyTChan chanResult) >>= \isEmpty
	 -> if isEmpty
	     then do
		gangState	<- getGangState gang
		if gangState == GangFinished
		 then	return ()
		 else do
			threadDelay 100000
			go_start

	     else do
		jobResult	<- atomically $ readTChan chanResult
		controller_handleResult config chainsTotal jobResult
		go_start


controller_handleResult config chainsTotal (JobResult chainIx jobIx job results)

	| ResultDiff fileRef fileOut fileDiff : _ <- [r | r@ResultDiff{} <- results]
	= do	printResult config chainsTotal chainIx jobIx job results
		
		putStr	$  "\n"
			++ "-- Output Differs  ----------------------------------------------------------\n"
			++ "   expected file: " ++ fileRef	++	"\n"
			++ "     actual file: " ++ fileOut	++ 	"\n"
			++ replicate 100 '-' ++ "\n"

		-- Show the difference
		str	<- readFile fileDiff
		putStr	str
		hFlush stdout
		return ()
		
	| otherwise
	= do	printResult config chainsTotal chainIx jobIx job results


printResult config chainsTotal chainIx jobIx job results
 = do	dirWorking	<- getCurrentDirectory
	let useColor	= not $ configBatch config
	let width	= configFormatPathWidth config

	putStrLn 
		$ render 
		$ parens (padR (length $ show chainsTotal)
				(ppr $ chainIx) 
				<> text "."
				<> ppr jobIx
				<> text "/" 
				<> ppr chainsTotal)
		<> space
		<> pprJobResult width useColor dirWorking job results

	hFlush stdout
