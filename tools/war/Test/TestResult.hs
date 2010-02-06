
module Test.TestResult
	( TestResult
	, Test(..)
	, pprTest 
	, NoShow(..))

where

import Config
import Format
import Test.TestFail
import Test.TestWin

import System.Exit



-- | A test will either fail or win.
type TestResult
	= Either TestFail TestWin


-- | The tests that we support.
data Test
	-- | Build a Main.ds file
	= TestBuild	 	FilePath

	-- | Build a Main.ds file expecting the build to fail.
	| TestBuildError	FilePath

	-- | Run a shell script
	| TestShell		FilePath

	-- | Run a shell script expecting it to fail (return nonzero)
	| TestShellError	FilePath

	-- | Compile and run a haskell program
	| TestHsBuild		FilePath

	-- | Compile a Module.ds file
	| TestCompile  	 	FilePath

	-- | Compile a Module.ds file expecting the compile to fail
	| TestCompileError	FilePath

	-- | Run an exectuable
	| TestRun	 	
		FilePath 			-- executable to run
		(NoShow (ExitCode -> Bool))	-- fn to decide whether test succeeded, based on exit code.

	-- | Diff an output file against the expected output
	| TestDiff      	FilePath FilePath	-- template, test output

	-- | Cleanup DDC generated files from this dir
	| TestClean		FilePath
	deriving Show
	
data NoShow a 
	= NoShow a

instance Show (NoShow a) where
	show x	= "NoShow"

instance Eq Test where
 (==) t1 t2
	=  testPath t1 == testPath t2
	&& testTag  t1 == testTag  t2
	
instance Ord Test where
 compare t1 t2
	= case compare (testPath t1) (testPath t2) of
		EQ	-> compare (testTag t1) (testTag t2)
		c	-> c
		
testPath test
 = case test of
	TestBuild 	 path		-> path
	TestHsBuild 	 path		-> path
	TestBuildError	 path		-> path
	TestShell        path		-> path
	TestShellError	 path		-> path
	TestCompile   	 path		-> path
	TestCompileError path		-> path
	TestRun 	 path _		-> path
	TestDiff	 temp out	-> out
	TestClean	 path		-> path


testTag test
 = case test of
	TestBuild{}		-> "build"
	TestHsBuild{}		-> "build"
	TestBuildError{}	-> "build error"
	TestShell{}		-> "shell"
	TestShellError{}	-> "shell error"
	TestCompile{}		-> "compile"
	TestCompileError{}	-> "compile error"
	TestRun{} 		-> "run"
	TestDiff{}		-> "diff"
	TestClean{}		-> "clean"
 

-- | Pretty print a test.
pprTest :: Test -> String
pprTest test
 = case test of	
	TestHsBuild	path 	-> padR formatPathWidth path ++ " build   "
	TestBuild	path 	-> padR formatPathWidth path ++ " build   "
	TestBuildError	path 	-> padR formatPathWidth path ++ " error   "
	TestShell       path 	-> padR formatPathWidth path ++ " shell   "
	TestShellError  path 	-> padR formatPathWidth path ++ " error   "
	TestCompile     path 	-> padR formatPathWidth path ++ " compile "
	TestCompileError path   -> padR formatPathWidth path ++ " error   "
	TestRun		path _	-> padR formatPathWidth path ++ " run     "
	TestDiff template out	-> padR formatPathWidth out  ++ " diff    "
	TestClean	path	-> padR formatPathWidth path ++ " clean   "

