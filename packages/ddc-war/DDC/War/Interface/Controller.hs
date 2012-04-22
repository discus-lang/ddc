
-- | Gang controller and user interface.
module DDC.War.Interface.Controller
	( ChanResult
	, controller)
where
import DDC.War.Interface.Config
import DDC.War.Driver
import BuildBox.Pretty
import BuildBox.Control.Gang
import System.IO
import System.Directory
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad
import Util.Terminal.VT100      as VT100
import Data.Maybe
import Data.List
import qualified System.Cmd


-- | Channel to write test job results to.
type ChanResult
	= TChan Result

	
-- | Gang controller and user interface for the war test driver.
--   This should be forked into its own thread, so it runs concurrently
--   with the gang that is actually executing the tests.
controller 
	:: Config
	-> Gang
	-> Int		-- ^ total number of chains
	-> ChanResult	-- ^ channel to receive results from
	-> IO [Result]

controller config gang chainsTotal chanResult
 = liftM reverse $ go_start []
 where	
	-- See if there is an input on the console.
        go_start :: [Result] -> IO [Result]
	go_start jobResults
	 =  hReady stdin >>= \gotInput
	 -> if gotInput
		then go_input jobResults
		else go_checkResult jobResults
	
	-- We've got input on the console, wait for already running tests then bail out.
	go_input jobResults
	 = do	putStrLn "Interrupt. Waiting for running jobs (CTRL-C kills)..."
	 	flushGang gang
	 	return jobResults

	-- See if any job results have been written to our input channel.
	go_checkResult jobResults
	 =  (atomically $ isEmptyTChan chanResult) >>= \isEmpty
	 -> if isEmpty
	     then do
		gangState	<- getGangState gang
		if gangState == GangFinished

		 -- No job results in channel, and the gang has finished.
		 -- We're done, so return to caller.
		 then do
			return jobResults

		 -- No job results in channel, but the gang is still running.
		 -- Spin until something else happens.
		 else do
			threadDelay 10000
			go_start jobResults

	     -- We've got a job result from the input channel.
	     -- Read the result and pass it off to the result handler.
	     else do
		jobResult <- atomically $ readTChan chanResult
		keepGoing <- handleResult config gang chainsTotal jobResult
		if keepGoing
		 then go_start (jobResult : jobResults)
		 else do
			killGang gang
			return (jobResult : jobResults)


-- | Handle a job result.
--   Returns True if the controller should continue, 
--   or False if we should shut down and return to the caller.
handleResult :: Config -> Gang -> Int -> Result -> IO Bool
handleResult config gang chainsTotal (Result chainIx jobIx job product)
 | ProductStatus jobName wayName testName status <- product
 = do   dirWorking      <- getCurrentDirectory
        let testName2    = fromMaybe testName  (stripPrefix dirWorking testName)
        let testName3    = fromMaybe testName2 (stripPrefix "/"        testName2)
        let width        = configFormatPathWidth config

        putStrLn 
         $ render 
         $ parens (padR (length $ show chainsTotal)
                        (ppr $ chainIx) 
                        <> text "."
                        <> ppr jobIx
                        <> text "/" 
                        <> ppr chainsTotal)
           <+> padL width (text testName3)
           <+> padL 8     (text wayName)
           <+> padL 8     (text jobName)
           <+> colorizeStatus config jobName (render status)

        hFlush stdout
        return True


 -- If a file is different than expected in batch mode,
 --   then just print the status.
 | ProductDiff jobName wayName testName _ _ _        <- product
 , configBatch config
 = handleResult config gang chainsTotal 
        $ Result chainIx jobIx job 
        $ ProductStatus testName wayName jobName (text "failed")


 -- If a file is different than expected in interactive mode,
 --   then ask the user what to do about it.
 | ProductDiff _ _ _ fileRef fileOut fileDiff <- product
 , not $ configBatch config
 = do	
	putStr	$  "\n"
		++ "-- Output Differs  -------------------------------------------------------------\n"
		++ "   expected file: " ++ fileRef	++	"\n"
		++ "     actual file: " ++ fileOut	++ 	"\n"
		++ replicate 80 '-' ++ "\n"

	-- Show the difference
	str	<- readFile fileDiff
	putStr	str
	hFlush stdout
	
	-- Ask the user what to do about it, 
	--  and pause the gang while we're waiting for user input
	pauseGang gang
	keepGoing	<- handleResult_askDiff fileRef fileOut fileDiff
	when keepGoing
	 $ resumeGang gang

	return keepGoing


handleResult_askDiff :: FilePath -> FilePath -> FilePath -> IO Bool
handleResult_askDiff fileRef fileOut fileDiff
 = do	putStr	$  replicate 80 '-' ++ "\n"
		++ "    (ENTER) continue   (e) show expected    (a) show actual\n"
		++ "    (CONTROL-C) quit   (u) update expected\n"
		++ "\n"
		++ "? "

	hFlush stdout
	cmd	<- hGetLine stdin

	let result
		-- continue, ignoring that the test gave a different result.
		| ""		<- cmd
		= return True

		-- Print the expected output
		| ('e': _)	<- cmd
		= do	str	<- readFile fileRef
			putStr	$  replicate 80 '-' ++ "\n"
			putStr	str
			handleResult_askDiff fileRef fileOut fileDiff

		-- Print the actual output
		| ('a': _)	<- cmd
		= do	str	<- readFile fileOut
			putStr	$  replicate 80 '-' ++ "\n"
			putStr	str
			handleResult_askDiff fileRef fileOut fileDiff

		-- Update the expected output with the actual one
		| ('u': _)	<- cmd
		= do	System.Cmd.system 
				$ "cp " ++ fileOut ++ " " ++ fileRef

			return True

		-- Invalid Command
		| otherwise
		= do	putStr	 $ "Invalid command.\n"
			handleResult_askDiff fileRef fileOut fileDiff
	
	result


colorizeStatus :: Config -> String -> String -> Doc
colorizeStatus config jobName status
        | configBatch config                   
        = text status

        | isPrefixOf "ok" status
        = colorDoc [Foreground Green] (text status)

        | isPrefixOf "success" status
        , jobName == "run"
        = colorDoc [Foreground Green] (text status)

        | isPrefixOf "success" status
        , jobName == "compile"
        = colorDoc [Foreground Blue]  (text status)

        | isPrefixOf "failed"  status
        = colorDoc [Foreground Red]   (text status)

        | otherwise
        = text status


colorDoc :: [VT100.Mode] -> Doc -> Doc
colorDoc mode doc
        = text 
        $ concat [ setMode mode
                 , render doc
                 , setMode [Reset] ]
