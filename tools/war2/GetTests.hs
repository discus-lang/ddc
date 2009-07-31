
module GetTests where

import Config
import Test
import TestNode
import Command
import War


import Util
import Util.FilePath
import Util.Data.WorkGraph		(WorkGraph, WorkNode(..))


-- Get Tests --------------------------------------------------------------------------------------
-- Look for tests in this directory
getTestsInDir :: Config -> DirPath -> War [TestNode]
getTestsInDir config dirPath
 = do	debugLn $ "- Looking for tests in " ++ dirPath

	-- See what files are here
	filesAll	<- lsFilesIn dirPath

	-- DDC dump files also end with .ds but we don't want to try and build them.
	--	Also skip over anything with the word "-skip" or "skip-" in it.
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

	let testsBuild
		= listWhen (gotMainDS && not gotMainErrorCheck && not gotMainSH)
		$ chainTests	[ TestBuild	(dirPath ++ "/Main.ds")
				, TestRun	(dirPath ++ "/Main.bin") ]

	let testsBuildError
		= listWhen (gotMainDS && gotMainErrorCheck && not gotMainSH)
		$ chainTests	[ TestBuildError (dirPath ++ "/Main.ds")
				, TestDiff	(dirPath ++ "/Main.error.check") 
						(dirPath ++ "/Main.compile.stderr") ]

	-- | If we have a Main.sh, then run that
	let testsShell
		= listWhen (gotMainSH && not gotMainErrorCheck)
		$ chainTests	[ TestShell	(dirPath ++ "/Main.sh") ]
		
	let testsShellError
		= listWhen (gotMainSH && gotMainErrorCheck)
		$ chainTests	[ TestShellError (dirPath ++ "/Main.sh")
				, TestDiff	(dirPath ++ "/Main.error.check") 
						(dirPath ++ "/Main.execute.stderr") ]


	-- If we ran an executable, and we have a stdout check file
	--	then check the executable's output against it
	let gotMainStdoutCheck	= any (isSuffixOf "/Main.stdout.check") files
	let testsStdout
		| gotMainStdoutCheck
		, Just nodeLast		<- takeLast testsBuild
		, tLast			<- testOfNode nodeLast
		= [node1 (TestDiff	(dirPath ++ "/Main.stdout.check")
					(dirPath ++ "/Main.stdout"))
			[tLast]]
		  
		| otherwise
		= []

	-- If there is no Main.ds then expect every source file that hasn't got an 
	--	associated error.check file to compile successfully.
	let testsCompile
		= listWhen (not gotMainDS && not gotMainSH)
		$ [ node1 (TestCompile file) []
				| file	<- filter (isSuffixOf ".ds") files 
				, let errorCheckFile	
					= (take (length file - length ".ds") file) ++ ".error.check"
				, not (elem errorCheckFile files)]

	-- If there is not Main.ds file then expect source files with an 
	--	associate error.check file to fail during compilation.
	let testsCompileError
		= listWhen (not gotMainDS && not gotMainSH)
		$ concat
		$ [ chainTests 
			[ TestCompileError file
			, TestDiff	   errorCheckFile compileStderr]
			
			| file	<- filter (isSuffixOf ".ds") files 
			, let fileBase		= baseNameOfPath file
			, let errorCheckFile	= fileBase ++ ".error.check"
			, let compileStderr	= fileBase ++ ".compile.stderr"
			, elem errorCheckFile files 
		  ]

	-- These tests are can be run in all ways
	--	So run them all ways that were specified
	let testsAllWays
		= concat 
			[ testsBuild,	testsBuildError
			, testsShell,	testsShellError
			, testsCompile,	testsCompileError
			, testsStdout ]

	let testsHereExpanded
		= expandWays (configWays config) testsAllWays

	-- Cleanup after each set of tests if asked 
	let testsFinal
		| configClean config
		, Just n1	<- takeLast $ testsHereExpanded
		= testsHereExpanded 
			++ [TestNode (TestClean dirPath, WayNil) [testIdOfNode n1]]

		| otherwise
		= testsHereExpanded


	debugLn 
		$  " Expandable tests " ++ dirPath ++ "\n"
		++ (unlines $ map show $ testsAllWays)
		++ " -- expanded --\n"
		++ (unlines $ map show $ testsHereExpanded)
		++ "\n"
		++ " -- final -- \n"
		++ (unlines $ map show $ testsFinal)
		++ "\n"


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
		$  mapM (getTestsInDir config) dirs

	return	$ testsFinal ++ moreTests



listWhen :: Bool -> [a] -> [a]
listWhen True xs	= xs
listWhen False xs	= []


justWhen :: Bool -> a -> Maybe a
justWhen True  x	= Just x
justWhen False _	= Nothing
