
import DDC.War.Options
import DDC.War.Way	()
import DDC.War.Config
import DDC.War.Job
import DDC.War.JobDispatch
import Util.Options
import Util.Options.Help
import BuildBox
import BuildBox.IO.Directory
import System.Environment
import System.Directory
import Control.Monad
import Util
import qualified Data.Sequence	as Seq
import qualified Data.Foldable	as Seq
import qualified Data.Set	as Set


main :: IO ()
main 
 = do	-- Parse command line options, and exit if they're no good.
	args	<- getArgs
	let (errs, options)	= parseOptions warOptions args
	let help		= makeOptionHelp 30 ["all"] warOptions 

	-- Print command usage if asked for.
	when (elem OptHelp options)
	 $ do	putStrLn $ help
		exitSuccess

	-- Print errors if there are any.
	when (not $ null errs)
	 $ do	putStrLn $ (catInt "\n" errs) 
		putStrLn $ help
		exitFailure

	-- Load war config from the cmd line options
	let config = loadConfig options
		
	-- All the starting test directories.
	testDirs
		<- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [dirs | OptTestDir dirs <- configOptions config]

	-- All the files in these directories.
	testFiles
		<- liftM (join . Seq.fromList)
		$ mapM traceFilesFrom testDirs
	
	let testFilesSet
		= Set.fromList $ Seq.toList testFiles
		
	let jobs
		= concat 
		$ map    (createJobs "normal" testFilesSet)
		$ filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
		$ Seq.toList testFiles

--	print jobs

	runBuildPrint "/tmp" $ mapM (dispatchJob config) jobs

	return ()


