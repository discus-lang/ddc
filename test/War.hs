{-# OPTIONS -fno-warn-missing-methods #-}


import System.Cmd
import System.Exit
import System.Time
import System.Environment
import System.IO
import System.Posix
import System.Directory

import Data.Char

import Util

data Arg
	= ArgFlagsDDC [String]
	| ArgFlagsGHC [String]
	| ArgDebug

---------------------------
main
 = do	
	let ?args	= 
		[ ArgFlagsDDC ["-link-lib m -lint -opt-tail-call"] ]

	let ?trace	= \s -> return ()
--	let ?trace	= \s -> do { putStr $ pretty s; return (); }

	out	$ "\n"
	
	args_	<- getArgs
	case args_ of
	 [] -> do	
		out	$ "* Building library.\n"
		(let 	?args = ?args ++ [ArgFlagsDDC ["-no-implicit-prelude"]]
		 in	mapM_ checkProg libraryOrder)
	 
		out	$ "\n"
		out	$ "* Running tests.\n"

		checkDir  "test/"

	 xx ->	do
	 	mapM_ checkDir xx
	 
	return ()

-----------------------
-- checkDir
--	path
--
checkDir path_
 | isInfixOf "-skip" path_
 = do
	out	$ "    " % padR 50 path_ % "(skipped)\n"
	return ()

 | otherwise
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
		mapM_ checkDir dirs
		
		when exist
		 $ 	out "\n"
		
		return	()

	 -- Otherwise look to see what's in this dir.
	 else do
		paths	<- getDirectoryContents path

		let sources
			= [ path ++ p
				| p	<- paths
				, let dirParts 	= splitOns '/' p
				, let fileParts	= splitOns '.' $ last dirParts
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

		mapM_ checkDir dirs
		mapM_ checkProg sources
		
		when exist
		 $	out "\n"
				 
	 	return	()

{-
isSourcePath path
 = let	dirParts	= splitOns '/' path
 	fileParts	= splitOns '.' $ last dirParts
	
   in	last fileParts == "ds"
 -}
   


-----------------------
-- checkProg
--
checkProg path
 = do
	let dirParts	= splitOns '/' path
	let dir		= concat $ init dirParts
	
	let fileParts	= splitOns '.' $ last dirParts
	let file	= concat $ init fileParts
	
	let pathB	= dir ++ file
	
	-- If the file is called "Main.ds" then we'll build a executable
	--	and run it. Otherwise just compile it to object code.
	
	let isMain	= if last dirParts == "Main.ds"
				then True
				else False

	-- do the tests
 	out	$ "    " % (padR 50 path) 
	
	checkProgComp pathB isMain

	out	$ "\n"



-----
-- checkProgComp
--	Compile the source.
--	
checkProgComp pathB isMain
 = do
	-- See if we've got an .error.check file, if so we're expecting the 
	--	compilation to fail with some sort of sensible error message.
	
	exist	<- fileExist (pathB ++ "error.check")
	
	if exist 
	 then checkProgCompError pathB isMain
	 else checkProgCompOk    pathB isMain
	 

checkProgCompOk pathB isMain
 = do
	?trace	$ "* checkProgCompOk " % pathB % " " % isMain % "\n"

	-- compile, expect success
	timeCompileStart	<- getClockTime

	let flags	
		= concat 
		$ [ catMap (\f -> " " ++ f) fs | ArgFlagsDDC fs <- ?args]
		
	systemE $ "bin/ddc"
		++ " -i library "
		++ (if isMain 
			then " -m " ++ (pathB ++ "ds") ++ " -o " ++ (pathB ++ "bin")
			else " -c " ++ (pathB ++ "ds"))
		++ flags

	timeCompileEnd		<- getClockTime

	out	$ "comp("
		% (padL 6 $ pretty $ timeCompileEnd - timeCompileStart)
		% "s)"


	-- more tests
	checkProgDI pathB
	
	when isMain
	 $ do	checkProgExec pathB
	 	checkProgStdout pathB


checkProgCompError pathB isMain
 = do
	?trace	$ "* checkProgCompError " % pathB % " " % isMain % "\n"

 	-- compile, expect error
	timeCompileStart	<- getClockTime
	systemE $ "bin/ddc"
		++ " -i library "
		++ (if isMain 
			then " -m " ++ (pathB ++ "ds") ++ " -o " ++ (pathB ++ "bin")
			else " -c " ++ (pathB ++ "ds"))
		++ " -stop-errors " ++ (pathB ++ "error.out")

	timeCompileEnd		<- getClockTime

	out	$ "comp("
		% (padL 6 $ pretty $ timeCompileEnd - timeCompileStart)
		% "s)"
	
	checkDiff
		(pathB ++ "error.out")
		(pathB ++ "error.check")
		(pathB ++ "error.diff")
		
	out	$ "  error(OK)"


-----
-- checkProgDI
--	Check module interface.
--
checkProgDI pathB
 = do
 	exist	<- fileExist (pathB ++ "di.check")
	
	when exist
	 $ do 	checkDiff 
	 		(pathB ++ "di") 
	 		(pathB ++ "di.check")
			(pathB ++ "di.diff")
			
		out	$ "  di(OK)"

-----
-- checkProgExec
--	Execute the binary. 
--
checkProgExec pathB 
 = do
 	-- See if there is a custom script for running this binary
	exist	<- fileExist (pathB ++ "exec.sh")
	
	if exist
	 then checkProgExec_custom  pathB
	 else checkProgExec_default pathB
	 
checkProgExec_custom pathB
 = do
 	-- run the script
	timeExecuteStart	<- getClockTime
	systemE $ (pathB ++ "exec.sh")
		++ " > " 
		++ (pathB ++ "stdout.out")

	timeExecuteEnd		<- getClockTime

	out	$ "  exec(" 
		% (padL 6 $ pretty $ timeExecuteEnd - timeExecuteStart)
		% "s)"
	


checkProgExec_default pathB
 = do
	-- run binary
	timeExecuteStart	<- getClockTime
	systemE $ (pathB ++ "bin")
		++ " > " 
		++ (pathB ++ "stdout.out")

	timeExecuteEnd		<- getClockTime

	out	$ "  exec(" 
		% (padL 6 $ pretty $ timeExecuteEnd - timeExecuteStart)
		% "s)"

-----
-- checkProgStdout
--	Check against what was emitted to stdout when we ran the binary.
--
checkProgStdout pathB
 = do	
 	exist	<- fileExist (pathB ++ "stdout.check")
	
	when exist
	 $ do	checkDiff
	 		(pathB ++ "stdout.out")
			(pathB ++ "stdout.check")
			(pathB ++ "stdout.diff")
			
		out	$ "  stdout(OK)"
	

-----------------------
-- checkDiff
--
checkDiff ::	FilePath -> FilePath -> FilePath -> IO ()
checkDiff    	fileOut     fileCheck   fileDiff
 = do
 	-- check output
	codeDiff 
		<- system $ "diff " ++ fileOut ++ " " ++ fileCheck ++ " > " ++ fileDiff
	
	case codeDiff of
	 ExitSuccess	-> return ()
	 _ 		-> checkDiff2 fileOut fileCheck fileDiff
	 

checkDiff2 :: 	FilePath -> FilePath -> FilePath	-> IO ()
checkDiff2	fileOut fileCheck fileDiff
 = do
 	-- Something isn't the same
	--	ask the user whether we should allow this change.
	--
	out	$ "\n\n"
		% "-------------------------------------------------------------------\n"
		% "! Output differs!\n"
		% "  check was between " % fileOut	% "\n"
		% "                and " % fileCheck 	% "\n"
		% "-------------------------------------------------------------------\n"

	str	<- readFile fileDiff
	putStr	str
	putStr "\n"

	out	$ "-------------------------------------------------------------------\n"
		% "Allow this?  (y)es (n)o (s)kip\n"
			
	ll	<- hGetLine stdin
	
	case ll of

	 -- change is ok, copy output over check file.
	 ('y' : _) 
	  -> do
	  	system	$ "cp " ++ fileOut ++ " " ++ fileCheck
		return ()

	 -- change is no good, bail out.
	 ('n' : _)
	  ->	exitFailure

	 -- skip this problem this time.
	 ('s' : _)	
	  -> 	return ()

	 -- don't allow the change.
	 _		-> exitFailure


-----
out s	
 = do	putStr $ pretty s
 	hFlush stdout


-----
systemE command
 = do
	?trace	$ "* systemE " % command	% "\n"
 	code	<- system command
	case code of
	 ExitFailure _	-> exitFailure
	 _		-> return ()


-----
instance Pretty ClockTime where
 prettyp (TOD sec_ psec_)
  = let	psecs	= sec_ * 10^12 + psec_
    in	(psecs `div` 10^12) % "." % (take 3 $ padLc '0' 12 $ show $ psecs `mod` 10^12)

instance Num ClockTime where
 (TOD s1 p1) - (TOD s2 p2)	= TOD (s1 - s2) (p1 - p2)
 (TOD s1 p1) + (TOD s2 p2)	= TOD (s1 + s2) (p1 + p2)	 
	
instance Pretty Integer where
 pretty = show



-----
libraryOrder
	= map (\s -> "library/" ++ s ++ ".ds")
	$ libraryModules
	
libraryModules = 
	[ "Base"
	, "Data/Ref"
	, "Base/Thunk"
	, "Data/Bool"
	, "Class/Eq"
	, "Class/Ord"
	, "Class/Num"
	, "Class/Update"
	, "Data/Int"
	, "Data/Int32U"
	, "Data/String"
	, "Data/Float32U"
	, "Data/Float"
	, "Data/Tuple"
	, "Data/Char"
	, "Data/Maybe"
	, "System/Error"
	, "System/Console"
	, "Data/List"
	, "Class/Show"
	, "Control/Imperative"
	, "Control/Exception"
	, "Data/Array"
	, "Data/ArrayU"
	, "DDC/Runtime"
	, "System/File"
	, "Prelude" ]
{-	, "Math/Util"
	, "Math/Vec2"
	, "Math/Matrix/Matrix33"
	, "Graphics/Primitive"
	, "Graphics/Shape"
	, "Graphics/TinyPTC" 
	, "Graphics/Raster/Bresenham"
	, "Graphics/Frame"
	, "Graphics/Render" 
	, "DDC/Source/Token"
	, "DDC/Source/Lexer" ]
-}	
