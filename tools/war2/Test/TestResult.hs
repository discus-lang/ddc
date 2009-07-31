
module Test.TestResult
	( TestResult
	, Test(..)
	, pprTest )

where

import Config
import Format
import Test.TestFail
import Test.TestWin



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

	-- | Compile a Module.ds file
	| TestCompile  	 	FilePath

	-- | Compile a Module.ds file expecting the compile to fail
	| TestCompileError	FilePath

	-- | Run an exectuable
	| TestRun	 	FilePath

	-- | Diff an output file against the expected output
	| TestDiff      	FilePath FilePath	-- template, test output
	deriving (Eq, Show)


instance Ord Test where
 compare t1 t2
	= case compare (testPath t1) (testPath t2) of
		EQ	-> compare (testTag t1) (testTag t2)
		c	-> c
		
testPath test
 = case test of
	TestBuild 	 path		-> path
	TestBuildError	 path		-> path
	TestShell        path		-> path
	TestShellError	 path		-> path
	TestCompile   	 path		-> path
	TestCompileError path		-> path
	TestRun 	 path		-> path
	TestDiff	 temp out	-> out


testTag test
 = case test of
	TestBuild{}		-> "build"
	TestBuildError{}	-> "build error"
	TestShell{}		-> "shell"
	TestShellError{}	-> "shell error"
	TestCompile{}		-> "compile"
	TestCompileError{}	-> "compile error"
	TestRun{} 		-> "run"
	TestDiff{}		-> "diff"
 

-- | Pretty print a test.
pprTest :: Test -> String
pprTest test
 = case test of	
	TestBuild	path 	-> padR formatPathWidth path ++ " build   "
	TestBuildError	path 	-> padR formatPathWidth path ++ " error   "
	TestShell       path 	-> padR formatPathWidth path ++ " shell   "
	TestShellError  path 	-> padR formatPathWidth path ++ " error   "
	TestCompile     path 	-> padR formatPathWidth path ++ " compile "
	TestCompileError path  -> padR formatPathWidth path ++ " error   "
	TestRun		path 	-> padR formatPathWidth path ++ " run     "
	TestDiff template out	-> padR formatPathWidth out  ++ " diff    "

