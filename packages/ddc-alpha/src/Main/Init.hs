
module Main.Init
	( verbLocateRunLib )
where
import DDC.Module.Error
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Main.Arg		(Arg)
import Util
import Util.System.Directory
import System.Directory
import qualified DDC.Main.Arg	as Arg


-- locate the path to the DDC runtime library and base libraries,
--	or die with an error if they can' be found
verbLocateRunLib
	:: Bool
	-> [Arg]
	-> IO (FilePath, FilePath)

verbLocateRunLib verbose args
 = do	-- this is the base of our installation
	--	it should contain the /runtime and /library subdirs.
	let pathBase_	= [dirs | Arg.PathBase dirs	<- args]

	-- if no base path is specified then look in the current directory.
	let pathBase 	= if pathBase_ == []
				then ["."]
				else pathBase_

	let pathLibrary_test = map (++ "/packages/ddc-alpha/library") pathBase
	let pathRuntime_test = map (++ "/packages/ddc-alpha/runtime") pathBase

	-- use the pathBase args and see if we can find the base library and the runtime system.
	mPathRuntime	<- liftM (liftM fst)
			$  findFileInDirs pathRuntime_test
			$ "libddc-runtime.a"

	mPathLibrary	<- liftM (liftM fst)
			$  findFileInDirs pathLibrary_test
			$ "Base.ds"

	-- if /runtime and /library can't be found then die with an appropriate error
	when (isNothing mPathRuntime)
	 $ exitWithUserError args
		[ ErrorCantFindRuntime pathRuntime_test ]

	when (isNothing mPathLibrary)
	 $ exitWithUserError args
		[ ErrorCantFindLibrary pathLibrary_test ]

	let Just pathRuntime_	= mPathRuntime
	let Just pathLibrary_	= mPathLibrary

	pathRuntime	<- canonicalizePath pathRuntime_
	pathLibrary	<- canonicalizePath pathLibrary_

	when verbose
	 $ do	putStr	$ pprStrPlain
			$ "  * Located Runtime and Libraries\n"
			% "    - pathRuntime = " % pathRuntime	% "\n"
	 		% "    - pathLibrary = " % pathLibrary	% "\n"
			% "\n"

	return 	( pathRuntime
		, pathLibrary)
