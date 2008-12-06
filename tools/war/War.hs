{-# LANGUAGE ImplicitParams #-}
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
import Data.Either
import Data.IORef
import Util
import Par


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
--	let ?trace	= \s -> do { putStr $ pprStrPlain s; return (); }

	let testDirs	= concat $ [dirs | ArgTestDirs dirs <- args]
            nThreads	= head   $ [n    | ArgThreads  n    <- args] ++ [1]

	if isNil testDirs
	    then do (libs,tests) <- testDefault args 
                    runTests libs  1 False  -- some libs really do depend on previous ones
                    runTests tests nThreads False
	    else do tests <- testSome args testDirs
                    runTests tests nThreads False
		
-- do the default tests
testDefault :: (?trace::PrettyM PMode -> IO ())
            => [Arg] -> IO ([IO Result], [IO Result])
testDefault args 
 = do	out	$ "\n"
	out	$ "* Running default tests.\n"
	
	-- switch on these args when doing the default tests
	let argsForce	= [ ArgFlagsDDC ["-l m -opt-tail-call -quiet"] ]
	let args'	= args ++ argsForce

	libs <- buildLibrary args' 

	let ?args	= args'
	tests <- enterDir "test/"
        return (libs, tests)

-- do some tests
testSome :: (?trace::PrettyM PMode -> IO ())
         => [Arg] -> [FilePath] -> IO [IO Result]
testSome args testDirs
 = do	out	$ "\n"
 	out	$ "* Running tests.\n"
	
	let argsForce	= [ ArgFlagsDDC ["-l m -opt-tail-call -quiet"] ]
	let ?args	= args ++ argsForce
	
	-- enter the test dirs
	tests <- mapM enterDir testDirs
	return $ concat tests


collectOut
	:: (?args :: [Arg])
	=> ((?trace :: PrettyM PMode -> IO ()) ->
            (?out   :: PrettyM PMode -> IO ()) => FilePath -> IO ())
	-> FilePath
	-> IO Result
collectOut func path
 = do	[traceR,outputR] <- replicateM 2 (newIORef [])
	let ?trace = \s -> modifyIORef traceR  (++ pprStrPlain s)
	let ?out   = \s -> modifyIORef outputR (++ pprStrPlain s)
	func path
	trace  <- readIORef traceR
	output <- readIORef outputR
	return $ Result True [(path, output++"\n", trace++"\n")]


-- Build the base library
buildLibrary :: (?trace::PrettyM PMode -> IO ())
             => [Arg] -> IO [IO Result]
buildLibrary args
 = do	out	$ "* Building library.\n"

	-- force on implicit prelude when compiling the base libs
	let ?args	= args ++ [ArgFlagsDDC ["-no-implicit-prelude -quiet"]]

	-- build the sources
	return $ map (collectOut testSource) (libraryOrder args)


-- enterDir ----------------------------------------------------------------------------------------
-- | Enter a directory, grab tests and run them.

enterDir 
	:: (?args :: [Arg])
	-> (?trace :: PrettyM PMode -> IO ())
	=> FilePath -> IO [IO Result]
enterDir path_

	-- if the path name contains -skip then skip over the whole dir
	| isInfixOf "-skip" path_ || isInfixOf "skip-" path_
	= do	out	$ "    " % padL 60 path_ % "(skipped)\n"
		return []

	| otherwise
 	= enterDir2 path_
	
enterDir2 :: (?args::[Arg])
          -> (?trace::PrettyM PMode -> IO ())
          => FilePath -> IO [IO Result]
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

	-- We've got an explit war.order, so follow that.
	--
	if exist
	 then do
	 	?trace		$ "order" % "\n"
		order		<- readFile (path ++ "war.order")
		
		let ll		= filter (\s -> not $ and $ map isSpace s)
				$ lines order
		
		?trace		$ show ll	% "\n"

		-- Find any other directories and enter them after the listed ones.
		paths		<- getDirectoryContents path
		otherDirs	<- filterM (doesDirectoryExist . (path++))
				$ [ p	| p@(p1:_)	<- paths
					, p /= "." && p /= ".." && p `notElem` ll]

		?trace		$ show otherDirs % "\n"

		let dirs	= map (\s -> path ++ s ++ "/") $ ll ++ otherDirs
	
--		out	$ "* Entering " % path	% "\n"
		tests <- mapM enterDir dirs
--		out	$ "\n"		

		return	$ concat tests

	 -- Otherwise look to see what's in this dir.
	 else do
		paths	<- getDirectoryContents path

		let sourcesAll
			= sort
			$ [ path ++ p
				| p	<- paths
				, let dirParts 	= chopOnRight '/' p
				, let fileParts	= chopOnRight '.' $ last dirParts
				, last fileParts == "ds"
				, length fileParts == 2 ]

		-- if there is a Main.ds in this dir, then don't compile the others
		--	leave that to the recursive -make
		let isMain p
			= (last $ chopOnRight '/' p) == "Main.ds"

		let (sourcesMain, sourcesOther)
			= partition isMain sourcesAll

		let sources
			| null sourcesMain	= sourcesOther
			| otherwise		= sourcesMain
		
		dirs	<- filterM doesDirectoryExist 
			$ [ path ++ p ++ "/"
				| p@(p1:_)	<- paths
				, p /= "." && p /= ".."]
--				, isUpper p1 ]
				
		?trace	$ "paths    = " % paths		% "\n"
			% "sources  = " % sources	% "\n"
			% "dirs     = " % dirs		% "\n"
			% "\n"

--		when (null sources)
--		 $ out	$ "* Entering " % path	% "\n"

		let tests = map (collectOut testSource) sources

		subtests <- mapM enterDir dirs

--		when (null sources)
--		 $ out	$ "\n"
		
	 	return	$ tests ++ concat subtests


	



