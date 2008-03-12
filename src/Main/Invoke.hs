module Main.Invoke 
	( invokeSeaCompiler
	, invokeLinker)

where

import Main.Arg
import Shared.Pretty
import Shared.Error
import Util

import System.Cmd
import System.Exit

-----
stage	= "Main.Invoke"

-----
-- | Invoke the external C compiler to compile this source program.

invokeSeaCompiler 
	:: (?args :: [Arg])
	-> FilePath		-- base path of source file
	-> FilePath		-- path to the runtime system
	-> FilePath		-- path to the base libraries
	-> [FilePath]		-- extra include dirs
	-> [String]		-- extra flags to compile with (from build files)
	-> IO ()

invokeSeaCompiler 
	pathSourceBase
	pathRuntime
	pathLibrary
	importDirs
	extraFlags
 = do
	let cmd	= "gcc"
		++ " -Werror"
		++ " -std=c99"

		++ (if elem Debug ?args
			then " -g"
			else "")

		++ (if elem OptAll ?args
			then " -O3"
			else "")

		++ (if elem Profile ?args
			then " -pg"
			else "")

--		++ " -Wall -Werror"

		++ " -I."

		++ " -I"  ++ (take (length pathRuntime - length "/runtime") pathRuntime)
		++ " -I"  ++ pathLibrary
		++ (concat [ " -I" ++ p | p <- importDirs ])

		++ " -c " ++ (pathSourceBase ++ ".ddc.c")
		++ " -o " ++ (pathSourceBase ++ ".o")
		++ " " ++ catInt " " extraFlags

 	when (elem Verbose ?args)
	 (do
		putStr	$ "\n"
	 	putStr	$ " * Invoking C compiler.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n")
		
	retCompile	<- system cmd
	
	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeSeaCompiler: compilation of C file failed.\n"
		% "    pathC = " % pathSourceBase % ".ddc.c" % "\n"
		


-- | Invoke the external linker to link these objects
invokeLinker 
	:: (?args :: [Arg])
	-> FilePath		-- ^ path to the runtime system
	-> [String]		-- ^ more libs to link with
	-> [String]		-- ^ more lib dirs to search
	-> [String]		-- ^ more objs to link with
	-> [FilePath]		-- ^ paths of interfaces of all modules to link
	-> IO ()

invokeLinker 
	pathRuntime
	moreLinkLibs
	moreLinkLibDirs
	moreLinkObjs
	objects
 = do
	let outFileName	
		= case filter (\x -> x =@= OutputFile{}) ?args of
			[OutputFile [fileName]] 	-> fileName
			_				-> "a.out"

	let moreObjs	= concat $ [files 	| LinkObj 	files 	<- ?args]
	let linkLibs	= concat $ [libs	| LinkLib 	libs	<- ?args]
	let linkLibDirs	= concat $ [dirs	| LinkLibDir	dirs	<- ?args]
			
	let cmd = "gcc"
		++ " -std=c99"
		++ " -o " ++ outFileName

		++ (if elem Profile ?args 
			then " -pg" 
			else "")

		++ " " ++ (catInt " " $ objects ++ moreObjs ++ moreLinkObjs)
					
		++ (if elem StaticRuntime ?args 
			then " " ++ pathRuntime ++ "/ddc-runtime.a"
			else " " ++ pathRuntime ++ "/ddc-runtime.so")

		-- most everything needs the math library, so always link against it
		++ " -lm"

		++ " " ++ (catInt " " ["-L" ++ dir | dir <- linkLibDirs ++ moreLinkLibDirs])
		++ " " ++ (catInt " " ["-l" ++ lib | lib <- linkLibs 	++ moreLinkLibs])

	when (elem Verbose ?args)
	 (do
	 	putStr	$ "\n"
		putStr	$ " * Invoking linker.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n")
		
	retLink		<- system cmd

	case retLink of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLinker: link failed\n"
		% "     objects:\n"
		% (catMap (\s -> pprStrPlain $ "        " % s % "\n") objects) % "\n"

	return ()

