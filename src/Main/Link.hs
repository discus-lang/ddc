
module Main.Link
	(linkFile)
where
	
import Main.BuildFile
import Main.Setup
import qualified Main.Arg	as Arg

import qualified Config.Config	as Config

import Shared.Error
import Shared.Pretty
import Util

import System.IO
import System.Exit
import System.Cmd
import System.Directory (doesDirectoryExist)

-----
stage	= "Main.Link"

-- Link -------------------------------------------------------------------------------------------
linkFile 
	:: Setup
	-> Maybe Build
	-> [FilePath]
	-> IO ()
	
linkFile setup mBuild objects
 = do	let ?verbose	= elem Arg.Verbose (setupArgsCmd setup)

	-- decide on an output file name to use
	let outFileName	
		= case filter (\x -> x =@= Arg.OutputFile{}) (setupArgsCmd setup) of
			[Arg.OutputFile fileName] 	-> fileName
			_				-> "a.out"

	-- extract extra link options from the command line args
	let args 	= setupArgsCmd setup
	let argObjs	= concat $ [files | Arg.LinkObj 	files 	<- args]
	let argLibs	= concat $ [libs  | Arg.LinkLib 	libs	<- args]
	let argLibDirs	= concat $ [dirs  | Arg.LinkLibDir	dirs	<- args]

	-- make the raw linker command
	--	The config file knows how to do this on our specific platform.
	let cmd	= Config.makeLinkCmd
			args
			(objects ++ argObjs ++ fromMaybe [] (liftM buildExtraLinkObjs mBuild))
			outFileName
			(setupRuntime setup)
			(argLibs 	++ fromMaybe [] (liftM buildExtraLinkLibs mBuild))
			(argLibDirs	++ fromMaybe [] (liftM buildExtraLinkLibDirs mBuild))

	when (elem Arg.Verbose args)
	 $ do	putStr	$ "\n"
		putStr	$ " * Invoking linker.\n"
		putStr	$ "   - command      = \"" ++ cmd ++ "\"\n"

	isDir		<-  doesDirectoryExist outFileName

	when isDir (dieWithUserError ["A directory already exists with this name: " % outFileName])
    
	retLink		<- system cmd

	case retLink of
	 ExitSuccess	-> return ()
	 ExitFailure _
	  -> panic stage
	  	$ "invokeLinker: link failed\n"
		% "     objects:\n"
		% (catMap (\s -> pprStrPlain $ "        " % s % "\n") objects) % "\n"

	return ()
