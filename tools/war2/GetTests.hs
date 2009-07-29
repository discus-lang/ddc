
module GetTests where

import Test
import Command
import War

import Util
import Util.FilePath
import Util.Data.BackGraph		(BackNode(..))
import Util.Data.WorkGraph		(WorkGraph, WorkNode(..))


-- Get Tests --------------------------------------------------------------------------------------
-- Look for tests in this directory
getTestsInDir :: DirPath -> War [(Test, BackNode Test)]
getTestsInDir dirPath
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
	--	If we have an error.check file then we're expecting it to fail
	let gotMainDS		= any (isSuffixOf "/Main.ds") files
	let gotMainErrorCheck	= any (isSuffixOf "/Main.error.check") files
	let mTestsBuild
		= justWhen (gotMainDS && not gotMainErrorCheck)
		$ let t1	= TestBuild     (dirPath ++ "/Main.ds")
		      t2	= TestRun	(dirPath ++ "/Main.bin")
	 	  in  [ (t1, BackNode [])
		      , (t2, BackNode [t1]) ]

	-- If we have an error.check file then we're expecting failure
	--	Build the program, and assuming it does actually fail,
	--	check the output against the expected.
	let mTestsBuildError
		= justWhen (gotMainDS && gotMainErrorCheck)
		$ let t1	= TestBuildError (dirPath ++ "/Main.ds")
		      t2	= TestDiff       (dirPath ++ "/Main.error.check") (dirPath ++ "/Main.compile.stderr")
		  in  [ (t1, BackNode []) 
		      , (t2, BackNode [t1]) ]

	-- If we ran an executable, and we have a stdout check file
	--	then check the executable's output against it
	let gotMainStdoutCheck	= any (isSuffixOf "/Main.stdout.check") files
	let mTestsStdout
		| Just [ _, (t2, _) ]	<- mTestsBuild
		, gotMainStdoutCheck
		= let t3	= TestDiff
					(dirPath ++ "/Main.stdout.check")
					(dirPath ++ "/Main.stdout")
		  in Just [ (t3, BackNode [t2]) ]
		  
		| otherwise
		= Nothing

	-- If there is no Main.ds then expect every source file that hasn't got an 
	--	associated error.check file to compile successfully.
	let mTestsCompile
		= justWhen (not $ gotMainDS)
		$ [ (TestCompile file, BackNode [])
				| file	<- filter (isSuffixOf ".ds") files 
				, let errorCheckFile	
					= (take (length file - length ".ds") file) ++ ".error.check"
				, not (elem errorCheckFile files)]

	-- If there is not Main.ds file then expect source files with an 
	--	associate error.check file to fail during compilation.
	let mTestsCompileError
		= justWhen (not $ gotMainDS)
		$ concat
		$ [ let t1	= TestCompileError file
		        t2	= TestDiff	   errorCheckFile compileStderr
		    in	[ (t1, BackNode [])
			, (t2, BackNode [t1]) ]
				| file	<- filter (isSuffixOf ".ds") files 
				, let fileBase		= baseNameOfPath file
				, let errorCheckFile	= fileBase ++ ".error.check"
				, let compileStderr	= fileBase ++ ".compile.stderr"
				, elem errorCheckFile files 
		  ]

	let testsHere	= concat 
			$ catMaybes 
				[ mTestsBuild
				, mTestsBuildError
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

	-- Recurse into directories
	moreTests 
		<- liftM concat
		$  mapM getTestsInDir dirs

	return	$ testsHere ++ moreTests

justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing
