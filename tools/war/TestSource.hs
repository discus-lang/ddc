
module TestSource 
	(testSource)
	
where

import System.Posix
import System.Directory
import System.IO
import System.Time

import Diff
import Interface
import Bits
import Util

-- testSource --------------------------------------------------------------------------------------
-- | This the top level test function
--
--	First compile the source.
--	If it doesn't compile then check errors against any .error.check file.
--	Check the produced interface against any .di.check files.
--	If it is a Main.ds then run it.
--	If there is a .stdout.check file then test the output against that as well.
--
testSource 
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath -> IO ()
	
testSource path
 = do
	-- normalise path names
	let dirParts	= chopOnRight '/' path
	let dir		= concat $ init dirParts
	
	let fileParts	= chopOnRight '.' $ last dirParts
	let file	= concat $ init fileParts
	
	let pathB	= dir ++ file
	
	-- See if this file is a Main.ds file 
	let isMain	= if last dirParts == "Main.ds"
				then True
				else False

	-- If there is a error.check file we're expecting the compile to fail
	expectFail	<- fileExist (pathB ++ "error.check")
		
	(if expectFail 
		then testSourceFail path pathB isMain 
		else testSourceOK path pathB isMain)

	out	$ "\n"


-- testSourceOK ------------------------------------------------------------------------------------

-- | Test a source file, expecting the compile to succeed
testSourceOK 
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath 
	-> FilePath
	-> Bool
	-> IO ()

testSourceOK path pathB isMain
 = do	-- slurp out any compile setups specificied via -with options
 	let setups	= [setup | ArgTestWith setup <- ?args]
	
	case setups of
	 -- no setups, just use default options
	 [] -> do
		out	$ "    " % (padR 50 path) 	
		testSourceOK_single pathB isMain []
		return ()
	 
	 -- got some setups, do comparison testing
	 _	-> testSourceOK_comparison path pathB isMain setups


-- | Test the file with just the default compile options
testSourceOK_single pathB isMain moreDDCArgs
 = do 	-- compile the program
 	out	$ "comp("

 	compileTime	<- compileSourceOk pathB isMain moreDDCArgs

	out	$ (padL 6 $ pprStr $ compileTime)
		% "s)"

	-- check any .di file
	checkProgDI pathB
	
	-- if it was a Main.ds file then we're expecing an executable binary to be produced..
	mExecTime
	 <- if isMain 
	     then do
		out	$ "  exec(" 

	 	-- run the produced binary
		execTime	<- executeProg pathB
		
		out	$ (padL 6 $ pprStr $ execTime)
			% "s)"

		-- check any stdout file
		checkProgStdout pathB
	
		return $ Just execTime

	     else
	     	return $ Nothing

	return (compileTime, mExecTime)


-- | Test the file with these different compile options, and compare the performance 
testSourceOK_comparison path pathB isMain setups@(setupBase: setupOther)
 = do 	
  	-- do the tests
 	out	$ "  - Comparison: " % (padR 50 path) 

 	out	$ "\n"
	
	let argsForced	= concat $ [as | ArgFlagsDDC as <- ?args]

	-- do the baseline test
	out	$ "        baseline options: " % pprOpts (argsForced ++ setupBase) % "\n"
	out	$ "            running test: "
	
	(baseCompileTime, mBaseExecTime)
		<- testSourceOK_single pathB isMain setupBase
	out	$ "\n"

	-- do the comparison tests with the baseline
	mapM_ (testSourceOK_compare path pathB isMain baseCompileTime mBaseExecTime) 
		setupOther

	return ()

pprOpts []	= pNil
pprOpts ss	= ppr $ catInt " " $ ss
 

-- test this source file and print comparisons with the baseline
testSourceOK_compare path pathB isMain baseCompileTime mBaseExecTime moreDDCArgs 
 = do	let argsForced	= concat $ [as | ArgFlagsDDC as <- ?args]

 	out	$ "         comparison with: " % pprOpts (argsForced ++ moreDDCArgs) % "\n"
 	out	$ "            running test: " 
 
  	-- compile the program
 	out	$ "comp("

 	compileTime	<- compileSourceOk pathB isMain moreDDCArgs

	out	$ (padL 6 $ pprStr $ compileTime)
		% "s)"

	-- check any .di file
	checkProgDI pathB
	
	-- if it was a Main.ds file then we're expecing an executable binary to be produced..
	mExecTime 
	 <- if isMain 
	     then do
		out	$ "  exec(" 

	 	-- run the produced binary
		execTime	<- executeProg pathB
		
		out	$ (padL 6 $ pprStr $ execTime)
			% "s)"

		-- check any stdout file
		checkProgStdout pathB

		return $ Just execTime

	     else do
	     	return $ Nothing

	out	$ "\n" % replicate 70 ' '
	

	-- work out percent change in compile time
	let rCompile :: Double	= fromIntegral (psecsOfClockTime compileTime) 
				/ fromIntegral (psecsOfClockTime baseCompileTime)

	out	$ " " % (padL 6 $ take 4 $ pprStr $ rCompile)

	-- work out percentage change in exec time
	when isMain 
	 $ do	let Just execTime		= mExecTime
	 	let Just baseExecTime	= mBaseExecTime

		let rExec :: Double	= fromIntegral (psecsOfClockTime execTime) 
					/ fromIntegral (psecsOfClockTime baseExecTime)
	
		out	$ " " % (padL 6 $ take 4 $ pprStr $ rExec)

	
	out "\n"
	return ()


-- testSourceFail ----------------------------------------------------------------------------------

-- | Test a source file, expecting the compile to fail
testSourceFail
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath
	-> FilePath 
	-> Bool
	-> IO ()

testSourceFail path pathB isMain
 = do 	-- do the tests
 	out	$ "    " % (padR 50 path) 

 	compileTime	<- compileSourceFail pathB isMain

	-- print the compile time
	out	$ "comp("
		% (padL 6 $ pprStr $ compileTime)
		% "s)"
	
	-- check the error 
	checkDiff
		(pathB ++ "error.out")
		(pathB ++ "error.check")
		(pathB ++ "error.diff")

	-- if the diff went through this will have succeeded		
	out	$ "  error(OK)"
	return ()


-- Compile -----------------------------------------------------------------------------------------

-- | Compile a source file, expecting it to succeed
compileSourceOk 
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath 		-- base path
	-> Bool 		-- whether this is a main file
	-> [String]		-- more args to pass to ddc
	-> IO ClockTime		-- compile time
	
compileSourceOk pathB isMain moreDDCArgs
 = do
	?trace	$ "* compileSourceOk " % pathB % " " % isMain % "\n"

	-- work out the flags to pass to DDC
	let flags	
		= concat 
		$ [ catMap (\f -> " " ++ f) fs | ArgFlagsDDC fs <- ?args]

	-- compile, the source and expect success
	compileTime 
	  <- timeIO_
	  $ do	systemE $ "bin/ddc"
			++ " -i library "
			++ (if isMain 
				then " -m " ++ (pathB ++ "ds") ++ " -o " ++ (pathB ++ "bin")
				else " -c " ++ (pathB ++ "ds"))
			++ flags
			++ " " ++ catInt " " moreDDCArgs

	return compileTime
	
	
-- | Compile a source file, expecting it to fail with an error
compileSourceFail
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath 		-- base path
	-> Bool 		-- whether this is a main file
	-> IO ClockTime		-- compile time

compileSourceFail pathB isMain
 = do
	?trace	$ "* compileSourceFail " % pathB % " " % isMain % "\n"

 	-- compile, expect error
	compileTime
	 <- timeIO_
	 $ do	systemE $ "bin/ddc"
			++ " -i library "
			++ (if isMain 
				then " -m " ++ (pathB ++ "ds") ++ " -o " ++ (pathB ++ "bin")
				else " -c " ++ (pathB ++ "ds"))
			++ " -stop-errors " ++ (pathB ++ "error.out")

	return compileTime


-- Execute -----------------------------------------------------------------------------------------

-- | execute a compiled program
executeProg pathB 
 = do 	-- See if there is a custom script for running this binary
	exist	<- fileExist (pathB ++ "exec.sh")
	
	if exist
	 then checkProgExec_custom  pathB
	 else checkProgExec_default pathB
	 

-- | Execute a program via a custom script
checkProgExec_custom pathB
 = timeIO_ 
 	$ systemE $ ((pathB ++ "exec.sh")
			++ " > " 
			++ (pathB ++ "stdout.out"))

checkProgExec_default pathB
 = timeIO_
 	$ systemE $ ((pathB ++ "bin")
			++ " > " 
			++ (pathB ++ "stdout.out"))


-- Check -------------------------------------------------------------------------------------------

-- | Check module interface (if the check file exists)
checkProgDI pathB
 = do 	exist	<- fileExist (pathB ++ "di.check")
	when exist
	 $ do 	checkDiff 
	 		(pathB ++ "di") 
	 		(pathB ++ "di.check")
			(pathB ++ "di.diff")
			
		out	$ "  di(OK)"


-- | Check stdout (if a check file exists)
checkProgStdout pathB
 = do	exist	<- fileExist (pathB ++ "stdout.check")
	when exist
	 $ do	checkDiff
	 		(pathB ++ "stdout.out")
			(pathB ++ "stdout.check")
			(pathB ++ "stdout.diff")
			
		out	$ "  stdout(OK)"



