
import DDC.War.Options
import DDC.War.Way	()
import DDC.War.Config
import DDC.War.JobCreate
import DDC.War.JobDispatch
import Util.Options
import Util.Options.Help
import BuildBox
import System.Environment
import System.Directory
import Control.Monad
import Util
import qualified Data.Sequence		as Seq
import qualified Data.Foldable		as Seq
import qualified Data.Set		as Set
import qualified Data.Traversable	as Seq


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
		
	-- All the starting test directories from the command line.
	testDirs
		<- mapM (makeRelativeToCurrentDirectory <=< canonicalizePath)
		$  [dirs | OptTestDir dirs <- configOptions config]

	-- Trace all the files reachable from these directories.
	testFilesRaw
		<- liftM (join . Seq.fromList)
		$  mapM traceFilesFrom testDirs
		
	-- Canonicalize all the paths and put them in a set (which sorts them)
	testFilesSet
		<- liftM (Set.fromList . Seq.toList)
		$  Seq.mapM canonicalizePath
		$  testFilesRaw

	let testFilesSorted
		= Set.toList testFilesSet

	-- Create test jobs based on the files we have.
	let jobs
		= Seq.toList
		$ join
		$ Seq.fromList
		$ map	 (Seq.fromList)
		$ map    (createJobs "normal" testFilesSet)
		$ filter (not . isInfixOf "skip-")	-- skip over skippable files.
		$ filter (not . isInfixOf "-skip")
		$ filter (not . isInfixOf "war-")	-- don't look at srcs in copied build dirs.
		$ testFilesSorted

	runBuildPrint "/tmp" $ mapM (dispatchJob config) jobs

	return ()


