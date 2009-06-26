
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
	= TestBuildMain FilePath
	| TestRunBinary FilePath
	| TestCompile   FilePath
	| TestDiff      FilePath FilePath
	deriving (Eq, Show)

instance Ord Test where
 compare t1 t2
	= case compare (testPath t1) (testPath t2) of
		EQ	-> compare (testTag t1) (testTag t2)
		c	-> c
		
testPath test
 = case test of
	TestBuildMain path	-> path
	TestRunBinary path	-> path
	TestCompile   path	-> path
	TestDiff      temp out	-> out
	
testTag test
 = case test of
	TestBuildMain{}		-> "buildmain"
	TestRunBinary{} 	-> "runbinary"
	TestCompile{}		-> "compile"
	TestDiff{}		-> "diff"

-- | Pretty print a test.
pprTest :: Test -> String
pprTest test
 = case test of	
	TestBuildMain  path		-> " * " ++ padR formatPathWidth path ++ " build   "
	TestRunBinary  path		-> " * " ++ padR formatPathWidth path ++ " run     "
	TestCompile    path		-> " * " ++ padR formatPathWidth path ++ " compile "
	TestDiff       template out	-> " * " ++ padR formatPathWidth out  ++ " diff    "


