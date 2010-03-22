module Main.Invoke 
	(invokeSeaCompiler)
where
import Util
import System.Cmd
import System.Exit
import DDC.Main.Error
import DDC.Main.Arg		(Arg)
import qualified DDC.Main.Arg	as Arg
import qualified Config.Config	as Config

-----
stage	= "Main.Invoke"

-- | Invoke the external C compiler to compile this source program.
invokeSeaCompiler 
	:: [Arg]		-- ^ ddc command line args
	-> FilePath		-- ^ base path of source file
	-> FilePath		-- ^ path to the runtime system
	-> FilePath		-- ^ path to the base libraries
	-> [FilePath]		-- ^ extra include dirs
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeSeaCompiler 
	args
	pathSourceBase
	pathRuntime
	pathLibrary
	importDirs
	extraFlags
 = do
	let cmd	= Config.makeSeaCompileCmd 
			args
			pathSourceBase
			pathRuntime
			pathLibrary
			importDirs
			extraFlags

 	when (elem Arg.Verbose args)
	 $ do	putStr	$ "\n"
	 	putStr	$ " * Invoking C compiler.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n"
		
	retCompile	<- system cmd
	
	case retCompile of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeSeaCompiler: compilation of C file failed.\n"
		% "    pathC = " % pathSourceBase % ".ddc.c" % "\n"

