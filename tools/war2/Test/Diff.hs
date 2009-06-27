
module Test.Diff
	(testDiff)
where

import Test.TestResult
import Test.TestFail
import Test.TestWin
import War
import Command

import Util.FilePath
import Control.Monad.Error

-- | Build a program starting from a Main.ds file
testDiff :: Test -> War TestWin
testDiff test@(TestDiff exp out)
 = do	gotExp	<- liftIOF $ fileExists exp
	gotOut	<- liftIOF $ fileExists out

	let result
		| not $ gotExp
		= throwError $ TestFailMissingFile exp

		| not $ gotOut
		= throwError $ TestFailMissingFile out

		| otherwise
		= testDiff' test

	result

testDiff' test@(TestDiff exp out)
 = do	debugLn $ "* TestDiff " ++ exp ++ " " ++ out

	-- the base name of the output file
	let outBase	= baseNameOfPath out

	-- file to write the diff output to
	let outDiff	= outBase ++ ".diff"

	-- if there is an existing diff file then remove it
	liftIOF $ removeIfExists outDiff

	-- do the diff.
	--	If there is a difference then diff will have a non-zero
	--	return code, which looks like a failure to the system command.
	--	
	let cmd	= "diff"
		++ " " ++ exp
		++ " " ++ out
		++ " > " ++ outDiff
				
	debugLn $ "  * cmd = " ++ cmd
	_	<- tryWar $ liftIOF $ system cmd
	
	-- read the output file back
	outFile	<- liftIO $ readFile outDiff

	case outFile of
	 []	-> return TestWinDiff
	 _	-> throwError
			$ TestFailDiff
			{ testFailExpectedFile	= exp
			, testFailActualFile	= out
			, testFailDiffFile	= outDiff }

