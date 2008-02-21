
-- This is the DDC super-mega test driver

import Order
import Diff
import Interface
import Bits
import TestSource

import Shared.Pretty

import System.Cmd
import System.Exit
import System.Time
import System.Environment
import System.IO
import System.Posix
import System.Directory

import Data.Char
import Util


-- main --------------------------------------------------------------------------------------------
main
 = do	-- get the command line args
  	argStrings		<- getArgs
	let (errs, args)	= parseArgs argStrings

--	putStr	$ pprStr $ "args = " % args % "\n"
	
	-- die on bad args
	when (not $ isNil errs)
	 $ do	putStr $ "unknown options: " ++ show errs ++ "\n\n"
	 	exitFailure
	
	-- dodgy trace function
	let ?trace	= \s -> return ()
--	let ?trace	= \s -> do { putStr $ pprStr s; return (); }

	let testDirs	= concat $ [dirs | ArgTestDirs dirs <- args]

	if isNil testDirs
		then testDefault args 
		else testSome args testDirs
		
-- do the default tests
testDefault args 
 = do	out	$ "\n"
	out	$ "* Running default tests.\n"
	
	-- switch on these args when doing the default tests
	let argsForce	= [ ArgFlagsDDC ["-l m -opt-tail-call"] ]
	let args'	= args ++ argsForce

	buildLibrary args' 

	let ?args	= args'
	enterDir "test/"

-- do some tests
testSome args testDirs
 = do	out	$ "\n"
 	out	$ "* Running tests.\n"
	
	let argsForce	= [ ArgFlagsDDC ["-l m -opt-tail-call"] ]
	let ?args	= args ++ argsForce
	
	-- enter the test dirs
	mapM_ enterDir testDirs
	return ()


-- Build the base library
buildLibrary args
 = do	out	$ "* Building library.\n"

	-- force on implicit prelude when compiling the base libs
	let ?args	= args ++ [ArgFlagsDDC ["-no-implicit-prelude"]]

	-- build the sources
	mapM_ testSource libraryOrder
	return ()	


-- enterDir ----------------------------------------------------------------------------------------
-- | Enter a directory, grab tests and run them.

enterDir 
	:: (?args :: [Arg])
	-> (?trace :: PrettyM PMode -> IO ())
	=> FilePath -> IO ()

enterDir path_

	-- if the path name contains -skip then skip over the whole dir
	| isInfixOf "-skip" path_
	= do	out	$ "    " % padL 50 path_ % "(skipped)\n"
		return ()

	| otherwise
 	= enterDir2 path_
	
enterDir2 path_
 = do
	let path
		= case last path_ of 
			 '/'	-> path_
			 _	-> path_ ++ "/"

	?trace	$ "* checkDir " % path	% "\n"

	-- check if there is a war.order file
	--	telling us what order to run the tests in
	exist	<- fileExist (path ++ "war.order")

	when (exist)
	 $ 	out	$ "* Entering " % path	% "\n"
	

	-- We've got an explit war.order, so follow that.
	--
	if exist
	 then do
	 	?trace		$ "order" % "\n"
		order		<- readFile (path ++ "war.order")
		
		let ll		= filter (\s -> not $ and $ map isSpace s)
				$ lines order
		
		?trace		$ show ll	% "\n"

		let dirs	= map (\s -> path ++ s ++ "/") ll
		mapM_ enterDir dirs
		
		when exist
		 $ 	out "\n"
		
		return	()

	 -- Otherwise look to see what's in this dir.
	 else do
		paths	<- getDirectoryContents path

		let sources
			= sort
			$ [ path ++ p
				| p	<- paths
				, let dirParts 	= chopOnRight '/' p
				, let fileParts	= chopOnRight '.' $ last dirParts
				, last fileParts == "ds"
				, length fileParts == 2 ]
		
		dirs	<- filterM doesDirectoryExist 
			$ [ path ++ p ++ "/"
				| p@(p1:_)	<- paths
				, p /= "." && p /= ".."
				, isUpper p1 ]
				
		?trace	$ "paths    = " % paths		% "\n"
			% "sources  = " % sources	% "\n"
			% "dirs     = " % dirs		% "\n"
			% "\n"

		mapM_ enterDir dirs
		mapM_ testSource sources
		
		when exist
		 $	out "\n"
				 
	 	return	()


	



