
-- This is the DDC super-mega test driver

import Order
import Diff
import Interface
import Bits
import TestSource

import System.Cmd
import System.Exit
import System.Time
import System.Environment
import System.IO
import System.Posix
import System.Directory

import Data.Char
import Util


---------------------------
main
 = do	
	let ?args	= 
		[ ArgFlagsDDC ["-link-lib m -lint -opt-tail-call -opt-simplify"] ]

	let ?trace	= \s -> return ()
--	let ?trace	= \s -> do { putStr $ pprStr s; return (); }

	out	$ "\n"
	
	args_	<- getArgs
	case args_ of
	 [] -> do	
		out	$ "* Building library.\n"
		(let 	?args = ?args ++ [ArgFlagsDDC ["-no-implicit-prelude"]]
		 in	mapM_ testSource libraryOrder)
	 
		out	$ "\n"
		out	$ "* Running tests.\n"

		enterDir "test/"

	 xx ->	do
	 	mapM_ enterDir xx
	 
	return ()

-- enterDir ----------------------------------------------------------------------------------------
-- | Enter a directory, grab tests and run them.

enterDir 
	:: (?args :: [Arg])
	-> (?trace :: PrettyP -> IO ())
	=> FilePath -> IO ()

enterDir path_

	-- if the path name contains -skip then skip over the whole dir
	| isInfixOf "-skip" path_
	= do	out	$ "    " % padR 50 path_ % "(skipped)\n"
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
			= [ path ++ p
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


	



