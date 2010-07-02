module Llvm.Invoke
	(invokeLlvmCompiler
        ,invokeLlvmAssembler
	)
where
import Util
import System.Cmd
import System.Exit
import DDC.Main.Error
import DDC.Main.Arg		(Arg)
import qualified DDC.Main.Arg	as Arg
import qualified Config.Config	as Config

-----
stage	= "Llvm.Invoke"

-- | Invoke the external LLVM compiler to compile this LLVM Intermediate
--	Representation source program into native assembler.
invokeLlvmCompiler
	:: [Arg]		-- ^ ddc command line args
	-> FilePath		-- ^ base path of source file
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeLlvmCompiler
	args
	pathSourceBase
	extraFlags
 = do
 	-- let cmd = Config.makeLlvmCompileCmd 
	let cmd	=  "llc-2.7 "
		++ pathSourceBase ++ ".ddc.ll"
		++ " -o " ++ pathSourceBase ++ ".ddc.s"

	when (elem Arg.Verbose args)
	 $ do	putStr	$ "\n"
	 	putStr	$ " * Invoking IR compiler.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n"

	retCompile	<- system cmd

	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLlvmCompiler: compilation of IR file failed.\n"
		% "    pathC = " % pathSourceBase % ".ddc.ll" % "\n"



-- | Invoke the external assembler to compile this native assembler
--	source program into a native object file.
invokeLlvmAssembler
	:: [Arg]		-- ^ ddc command line args
	-> FilePath		-- ^ base path of source file
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeLlvmAssembler
	args
	pathSourceBase
	extraFlags
 = do
 	-- let cmd = Config.makeLlvmAssembleCmd 
	let cmd	=  "as "
		++ pathSourceBase ++ ".ddc.s"
		++ " -o " ++ pathSourceBase ++ ".o"

	when (elem Arg.Verbose args)
	 $ do	putStr	$ "\n"
	 	putStr	$ " * Invoking assembler.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n"

	retCompile	<- system cmd

	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLlvmAssembler: compilation of ASM file failed.\n"
		% "    pathC = " % pathSourceBase % ".s" % "\n"
