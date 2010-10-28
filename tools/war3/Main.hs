
import DDC.War.Options
import DDC.War.Way	()
import DDC.War.Config
import DDC.War.FileType
import Util.Options
import Util.Options.Help
import System.Environment
import System.Exit
import System.Directory
import Control.Monad
import Util
import qualified Data.Sequence	as Seq
import Data.Sequence		(Seq)
import qualified Data.Foldable	as Seq

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

	-- Check the types of the files
	let testFileTypes
		= [(file, classifyFile file) | file <- Seq.toList testFiles]

	print testFileTypes


-- | Get all the files reachable from this directory
-- TODO: Move this to buildbox.
traceFilesFrom :: FilePath -> IO (Seq FilePath)
traceFilesFrom path
 = do	isDir	<- doesDirectoryExist path
	isFile	<- doesFileExist      path

	let result
		| isDir 	
		= do	contents <- liftM (filter (\s -> not $ elem s [".", ".."]))
			 	 $  getDirectoryContents path

			liftM (join  . Seq.fromList)
				$ mapM traceFilesFrom 
				$ map (\f -> path ++ "/" ++ f) 
				$ contents

		| isFile
		=	return	$ Seq.singleton path
		
		| otherwise
		=	return	$ Seq.empty
	
	result




	













