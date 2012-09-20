module Sea.Invoke 
	(invokeSeaCompiler)
where
import Util
import System.Cmd
import System.Exit
import System.Directory
import DDC.Main.Error
import DDC.Main.Arg		(Arg)
import qualified DDC.Main.Arg	as Arg
import qualified Config.Config	as Config

stage	= "Sea.Invoke"

-- | Invoke the external C compiler to compile this source program.
invokeSeaCompiler 
	:: [Arg]		-- ^ ddc command line args
	-> FilePath		-- ^ path of source C file (must be canonical)
	-> FilePath		-- ^ path of output O file (must be canonical)
	-> FilePath		-- ^ path to the runtime system
	-> FilePath		-- ^ path to the base libraries
	-> [FilePath]		-- ^ extra include dirs
	-> [String]		-- ^ extra flags to compile with (from build files)
	-> IO ()

invokeSeaCompiler args
	pathC pathO
	pathRuntime pathLibrary
	importDirs
	extraFlags
 = do
	pathRuntime'	<- canonicalizePath pathRuntime
	pathLibrary'	<- canonicalizePath pathLibrary
	importDirs'	<- mapM canonicalizePath $ nub importDirs

	let cmd	= Config.makeSeaCompileCmd 
			args
			pathC
			pathO
			pathRuntime'
			pathLibrary'
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
		% "  command was = '" % cmd % "'\n"

