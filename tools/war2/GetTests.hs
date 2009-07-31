
module GetTests where

import Config
import Test
import Command
import War

import Util
import Util.FilePath
import Util.Data.BackGraph		(BackNode(..))
import Util.Data.WorkGraph		(WorkGraph, WorkNode(..))


-- Get Tests --------------------------------------------------------------------------------------
-- Look for tests in this directory
getTestsInDir :: Config -> DirPath -> War [((Test, Way), BackNode (Test, Way))]
getTestsInDir config dirPath
 = do	debugLn $ "- Looking for tests in " ++ dirPath

	-- See what files are there
	filesAll	<- lsFilesIn dirPath

	-- Dump files also end with .ds 
	--	but we don't want to try and build them...
	let files	= filter 
				(\name -> (not $ isInfixOf ".dump-" name) 
				       && (not $ isInfixOf "-skip"  name)
				       && (not $ isInfixOf "skip-"  name))
				filesAll

	-- Build and run executables if we have a Main.ds
	-- Execute shells scripts called Main.sh
	--	If we have an error.check file then we're expecting it to fail.

	let gotMainDS		= any (isSuffixOf "/Main.ds") files
	let gotMainSH		= any (isSuffixOf "/Main.sh") files
	let gotMainErrorCheck	= any (isSuffixOf "/Main.error.check") files

	let mTestsBuild
		= justWhen (gotMainDS && not gotMainErrorCheck && not gotMainSH)
		$ let n1	= (TestBuild	(dirPath ++ "/Main.ds"),  WayNil)
		      n2	= (TestRun	(dirPath ++ "/Main.bin"), WayNil)
	 	  in  [ (n1, BackNode [])
		      , (n2, BackNode [n1]) ]

	-- If we have an error.check file then we're expecting failure
	--	Build the program, and assuming it does actually fail,
	--	check the output against the expected.
	let mTestsBuildError
		= justWhen (gotMainDS && gotMainErrorCheck && not gotMainSH)
		$ let n1	= (TestBuildError (dirPath ++ "/Main.ds"), WayNil)
		      n2	= (TestDiff
					(dirPath ++ "/Main.error.check") 
					(dirPath ++ "/Main.compile.stderr")
					, WayNil)
		  in  [ (n1, BackNode []) 
		      , (n2, BackNode [n1]) ]

	-- | If we have a Main.sh, then run that
	let mTestsShell
		= justWhen (gotMainSH && not gotMainErrorCheck)
		$ let n1	= (TestShell	(dirPath ++ "/Main.sh"), WayNil)
		  in  [ (n1, BackNode []) ]

	let mTestsShellError
		= justWhen (gotMainSH && gotMainErrorCheck)
		$ let n1	= (TestShellError (dirPath ++ "/Main.sh"), WayNil)
		      n2	= (TestDiff	 (dirPath ++ "/Main.error.check") (dirPath ++ "/Main.execute.stderr"), WayNil)
		  in  [ (n1, BackNode [])
		      , (n2, BackNode [n1]) ]

	-- If we ran an executable, and we have a stdout check file
	--	then check the executable's output against it
	let gotMainStdoutCheck	= any (isSuffixOf "/Main.stdout.check") files
	let mTestsStdout
		| Just [ _, (n2, _) ]	<- mTestsBuild
		, gotMainStdoutCheck
		= let n3	= (TestDiff
					(dirPath ++ "/Main.stdout.check")
					(dirPath ++ "/Main.stdout"), WayNil)
		  in Just [ (n3, BackNode [n2]) ]
		  
		| otherwise
		= Nothing

	-- If there is no Main.ds then expect every source file that hasn't got an 
	--	associated error.check file to compile successfully.
	let mTestsCompile
		= justWhen (not gotMainDS && not gotMainSH)
		$ [ ((TestCompile file, WayNil), BackNode [])
				| file	<- filter (isSuffixOf ".ds") files 
				, let errorCheckFile	
					= (take (length file - length ".ds") file) ++ ".error.check"
				, not (elem errorCheckFile files)]

	-- If there is not Main.ds file then expect source files with an 
	--	associate error.check file to fail during compilation.
	let mTestsCompileError
		= justWhen (not gotMainDS && not gotMainSH)
		$ concat
		$ [ let n1	= (TestCompileError file, WayNil)
		        n2	= (TestDiff	   errorCheckFile compileStderr, WayNil)
		    in	[ (n1, BackNode [])
			, (n2, BackNode [n1]) ]
				| file	<- filter (isSuffixOf ".ds") files 
				, let fileBase		= baseNameOfPath file
				, let errorCheckFile	= fileBase ++ ".error.check"
				, let compileStderr	= fileBase ++ ".compile.stderr"
				, elem errorCheckFile files 
		  ]

	let testsHere	= concat 
			$ catMaybes 
				[ mTestsBuild
				, mTestsShell
				, mTestsBuildError
				, mTestsShellError
				, mTestsCompile
				, mTestsCompileError 
				, mTestsStdout ]

	-- See what dirs we can recurse into
	dirsAll		<- lsDirsIn dirPath
	
	-- Skip over boring dirs
	let dirs	= filter
				(\name -> (not $ isInfixOf "-skip" name)
				       && (not $ isInfixOf "skip-" name))
				dirsAll

	let testsHereExpanded
		= expandWays (configWays config) testsHere

	debugLn 
		$  " Tests in " ++ dirPath ++ "\n"
		++ (unlines $ map show $ testsHere)
		++ " -- expanded --\n"
		++ show testsHereExpanded
		++ "\n"

	-- Cleanup after each set of tests if asked 
	let testsFinal
		| configClean config
		, Just (t1, back)	<- takeLast $ testsHereExpanded
		= testsHereExpanded 
			++ [((TestClean dirPath, WayNil), BackNode [t1])]

		| otherwise
		= testsHereExpanded

	-- Recurse into directories
	moreTests 
		<- liftM concat
		$  mapM (getTestsInDir config) dirs


	return	$ testsFinal ++ moreTests


type Node
	= ( (Test, Way), BackNode (Test, Way) )

expandWays :: [Way] -> [Node] -> [Node]
expandWays ways nodes
	= let	newNodes	= map (\w -> map (patchWay_node w) nodes) ways
	  in	concat $ chainNodes newNodes


-- Chain a list of lists of nodes, so the first node
--	of each group depends on the last node of the previous one.
chainNodes :: [[Node]] -> [[Node]]
chainNodes []		= []
chainNodes (x : [])	= x : []

chainNodes ([] : y : rest)
	= [] : chainNodes (y : rest)

chainNodes (x  : [] : rest)
	= x : chainNodes ([] : rest)

chainNodes (x : y : rest)
 = let	Just (xTest, BackNode _)	= takeLast x

	Just (yTest, BackNode yBack)	= takeHead y
	Just yTail			= takeTail y

	yHead'				= (yTest, BackNode (xTest : yBack))
	y'				= yHead' : yTail

   in 	x : chainNodes (y' : rest)

	
patchWay_node :: Way -> Node -> Node
patchWay_node way ((test, _), BackNode testWays)
 = 	( (test, way)
	, BackNode (map (\(t, _) -> (t, way)) testWays) )


justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing
