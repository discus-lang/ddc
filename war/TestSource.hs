
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

	-- do the tests
 	out	$ "    " % (padR 50 path) 
	

	-- If there is a error.check file we're expecting the compile to fail
	expectFail	<- fileExist (pathB ++ "error.check")
		
	(if expectFail 
		then testSourceFail pathB isMain 
		else testSourceOK pathB isMain)

	out	$ "\n"


-- | Test a source file, expecting the compile to succeed
testSourceOK 
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath 
	-> Bool
	-> IO ()

testSourceOK pathB isMain 
 = do	
 	-- compile the program
 	compileTime	<- compileSourceOk pathB isMain
 	out	$ "comp("
		% (padL 6 $ pprStr $ compileTime)
		% "s)"

	-- check any .di file
	checkProgDI pathB
	
	-- if it was a Main.ds file then we're expecing an executable binary to be produced..
	when isMain
	 $ do	-- run the produced binary
		execTime	<- executeProg pathB
		out	$ "  exec(" 
			% (padL 6 $ pprStr $ execTime)
			% "s)"

		-- check any stdout file
		checkProgStdout pathB
	

-- | Test a source file, expecting the compile to fail
testSourceFail
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath 
	-> Bool
	-> IO ()

testSourceFail pathB isMain
 = do	compileTime	<- compileSourceFail pathB isMain

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
	-> IO ClockTime		-- compile time
	
compileSourceOk pathB isMain
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



