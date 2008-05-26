
module Main.Init
	( verbLocateRunLib )
where

import qualified Config.Config	as Config
import qualified Main.Arg	as Arg
import Main.Arg			(Arg)
import Module.IO
import Shared.Pretty
import Shared.Error
import Util


-- locate the path to the DDC runtime library and base libraries,
--	or die with an error if they can' be found
verbLocateRunLib 
	:: Bool
	-> [Arg]
	-> IO (FilePath, FilePath)

verbLocateRunLib verbose args
 = do	-- this is the base of our installation
	--	it should contain the /runtime and /library subdirs.
	let pathBase_	= concat [dirs | Arg.PathBase dirs	<- args]

	-- if no base path is specified then look in the current directory.
	let pathBase 	= if pathBase_ == []
				then ["."]
				else pathBase_

	let pathLibrary_test = map (++ "/library") pathBase
	let pathRuntime_test = map (++ "/runtime") pathBase

	-- use the pathBase args and see if we can find the base library and the runtime system.
	mPathRuntime	<- liftM (liftM fst)
			$  findFile pathRuntime_test
			$ "libddc-runtime." ++ Config.extSharedObject

	mPathLibrary	<- liftM (liftM fst)
			$  findFile pathLibrary_test
			$ "Base.ds"
	
	-- if /runtime and /library can't be found then die with an appropriate error
	when (isNothing mPathRuntime)
	 $ dieWithUserError 
	 	[ "Can't find the DDC runtime system.\n"
		% "    Please supply a '-basedir' option to specify the directory\n"
		% "    containing 'runtime/ddc-runtime.so'\n"
		% "\n"
	 	% "    tried:\n" %> ("\n" %!% pathRuntime_test) % "\n\n"
		% "    use 'ddc -help' for more information\n"]

	when (isNothing mPathLibrary)
	 $ dieWithUserError 
	 	[ "Can't find the DDC base library.\n"
		% "    Please supply a '-basedir' option to specify the directory\n"
		% "    containing 'library/Base.ds'\n"
		% "\n"
	 	% "    tried:\n" %> ("\n" %!% pathLibrary_test) % "\n\n"
		% "    use 'ddc -help' for more information\n"]
	
	
	let Just pathRuntime	= mPathRuntime
	let Just pathLibrary	= mPathLibrary
	
	when verbose
	 $ do	putStr	$ pprStrPlain
			$ "  * Located Runtime and Libraries\n"
			% "    - pathRuntime = " % pathRuntime	% "\n"
	 		% "    - pathLibrary = " % pathLibrary	% "\n"
			% "\n"
	
	return 	( pathRuntime
		, pathLibrary)
	