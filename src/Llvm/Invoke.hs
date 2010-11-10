module Llvm.Invoke
	( invokeLlvmCompiler
	, invokeLlvmAssembler )
where

import Main.Util

import Util
import System.Cmd
import System.Exit
import DDC.Main.Error

-----
stage	= "Llvm.Invoke"

-- | Invoke the external LLVM compiler to compile this LLVM Intermediate
--	Representation source program into native assembler.
invokeLlvmCompiler
	:: (?verbose :: Bool)
	=> FilePath		-- ^ base path of source file
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeLlvmCompiler
	pathSourceBase
	extraFlags
 = do
 	-- let cmd = Config.makeLlvmCompileCmd
	let cmd	=  "llc "
		++ pathSourceBase ++ ".ddc.ll"
		++ " -o " ++ pathSourceBase ++ ".ddc.s"

	outVerb $ ppr $ "\n"
		% "  * Invoking IR compiler.\n"
		% "    - command = \"" % cmd % "\"\n"

	retCompile	<- system cmd

	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLlvmCompiler: compilation of IR file failed.\n"
		% "    path = " % pathSourceBase % ".ddc.ll" % "\n"



-- | Invoke the external assembler to compile this native assembler
--	source program into a native object file.
invokeLlvmAssembler
	:: (?verbose :: Bool)
	=> FilePath		-- ^ base path of source file
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeLlvmAssembler
	pathSourceBase
	extraFlags
 = do
 	-- let cmd = Config.makeLlvmAssembleCmd
	let cmd	=  "as "
		++ pathSourceBase ++ ".ddc.s"
		++ " -o " ++ pathSourceBase ++ ".o"

	outVerb $ ppr $ "\n"
		% "  * Invoking assembler.\n"
		% "    - command = \"" % cmd % "\"\n"

	retCompile	<- system cmd

	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLlvmAssembler: compilation of ASM file failed.\n"
		% "    path = " % pathSourceBase % ".s" % "\n"

