
module Test.TestResult
	( TestResult
	, Test(..)
	, pprTest )

where

import Format
import Test.TestFail
import Test.TestWin


-- | A test will either fail or win.
type TestResult
	= Either TestFail TestWin

-- | The tests that we support.
data Test
	= TestBuild	 		FilePath
	| TestBuildError		FilePath
	| TestCompile  	 		FilePath
	| TestCompileError		FilePath
	| TestRun	 		FilePath
	| TestDiff      		FilePath FilePath
	deriving (Eq, Show)

instance Ord Test where
 compare t1 t2
	= case compare (testPath t1) (testPath t2) of
		EQ	-> compare (testTag t1) (testTag t2)
		c	-> c
		
testPath test
 = case test of
	TestBuild 	 path	  -> path
	TestBuildError	 path	  -> path
	TestCompile   	 path	  -> path
	TestCompileError path	  -> path
	TestRun 	 path	  -> path
	TestDiff	 temp out -> out
	
testTag test
 = case test of
	TestBuild{}			-> "build"
	TestBuildError{}		-> "build error"
	TestCompile{}			-> "compile"
	TestCompileError{}		-> "compile error"
	TestRun{} 			-> "run"
	TestDiff{}			-> "diff"

-- | Pretty print a test.
pprTest :: Test -> String
pprTest test
 = case test of	
	TestBuild	    	path	-> " * " ++ padR formatPathWidth path ++ " build   "
	TestBuildError		path	-> " * " ++ padR formatPathWidth path ++ " error   "
	TestCompile     	path	-> " * " ++ padR formatPathWidth path ++ " compile "
	TestCompileError	path	-> " * " ++ padR formatPathWidth path ++ " error   "
	TestRun		    	path	-> " * " ++ padR formatPathWidth path ++ " run     "
	TestDiff       template out	-> " * " ++ padR formatPathWidth out  ++ " diff    "

