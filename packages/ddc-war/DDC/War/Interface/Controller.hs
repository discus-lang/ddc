
-- | Gang controller and user interface.
module DDC.War.Interface.Controller
	( Config        (..)
	, controller)
where
import DDC.War.Driver
import BuildBox.Pretty
import BuildBox.Control.Gang
import System.IO
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM
import Control.Monad
import Data.Maybe
import Data.List
import DDC.War.Interface.VT100      as VT100
import qualified System.Cmd


data Config
        = Config
        { -- | Allow the user to control the driver from the console, 
          --   and ask what to do interactively if a file is different than expected.
          configInteractive     :: Bool 

          -- | Use VT100 colors in the output.
        , configColoredOutput   :: Bool

          -- | Pad test names out to this column width.
        , configFormatPathWidth :: Int 

          -- | Suppress this prefix from the front of displayed test names.
          --   Test names usually have fully qualified paths,
          --   but we wont want to display the "/Users/whoever/devel/code/ddc/test"
          --   prefix.
        , configSuppressPrefix  :: String }
        deriving Show

	

-- | Gang controller and user interface for the war test driver.
--   This should be forked into its own thread, so it runs concurrently
--   with the gang that is actually executing the tests.
controller 
	:: Config
	-> Gang
	-> Int		       -- ^ Total number of chains.
	-> TChan Result        -- ^ Channel to receive results from.
	-> IO [Result]

controller config gang chainsTotal chanResult
 = liftM reverse $ go_start []
 where	
	-- See if there is an input on the console.
        go_start :: [Result] -> IO [Result]
	go_start jobResults

         | configInteractive config
         = hReady stdin >>= \gotInput
           -> if gotInput
                then go_input jobResults
                else go_checkResult jobResults

         | otherwise
         = go_checkResult jobResults

	
	-- We've got input on the console, wait for already running tests then bail out.
	go_input jobResults
	 = do	putStrLn "Interrupt. Waiting for running jobs (CTRL-C kills)..."
	 	flushGang gang
	 	return jobResults


	-- See if any job results have been written to our input channel.
	go_checkResult jobResults
 	 =  (atomically $ isEmptyTChan chanResult) >>= \empty'
	 -> if empty'
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
handleResult config gang chainsTotal
        (Result chainIx jobIx jobId actionName product')
 | JobId testName wayName       <- jobId
 , ProductStatus status         <- product'
 = do   let testName2    = fromMaybe testName 
                                (stripPrefix (configSuppressPrefix config) testName)

        let width        = configFormatPathWidth config

        putStrLn 
         $ render 
         $ parens (padR (length $ show chainsTotal)
                        (ppr $ chainIx) 
                        <> text "."
                        <> ppr jobIx
                        <> text "/" 
                        <> ppr chainsTotal)
           <+> padL width (text testName2)
           <+> padL 5     (text wayName)
           <+> padL 8     (text actionName)
           <+> colorizeStatus config actionName (render status)

        hFlush stdout
        return True

 -- If a file is different than expected in interactive mode,
 --   then ask the user what to do about it.
 | ProductDiff fileRef fileOut fileDiff <- product'
 , configInteractive config
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

 -- If a file is different than expected in batch mode,
 --   then just print the status.
 | ProductDiff{}        <- product'
 = handleResult config gang chainsTotal 
        $ Result chainIx jobIx jobId actionName
        $ ProductStatus (text "failed")

 -- Bogus pattern match to avoid warning.
 | otherwise
 = error "DDC.War.Interface.Controller.handleResult: no match"


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
        | not $ configColoredOutput config
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
