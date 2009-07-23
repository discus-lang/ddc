{-# OPTIONS -fno-warn-missing-methods #-}


import System.Cmd
import System.Exit
import System.Time
import System.Environment
import System.IO
import Util

optFlags	= " -O "
	
---------------------------
main
 = do	args_	<- getArgs

	case args_ of
	 [] 
	  -> do	
	  	checkBase 
	  	checkError 
		checkStaged []
		checkDeFib  []
		
	 ("base":_)	-> checkBase
	 ("error":_)	-> checkError
	 ("staged":xs)	-> checkStaged xs
	 ("defib":xs)	-> checkDeFib  xs
	 	
 	 

-----------------------
-- checkBase
--	These modules form the basic library of the language.
--	If they don't compile cleanly then we're already dead.
--


checkBase
 = do
 	putStr "* Building base modules.\n"
	mapM	traumaC baseModules
	putStr "\n"


traumaC m
 = do
	let fileName	= "library/" ++ m ++ ".ds"
	putStr	$ "  - Compiling " ++ fileName ++ "\n"
	systemE	$ "bin/ddc"
			++ " -i library -no-implicit-prelude"
			++ " -c " ++ fileName



-----------------------
-- check
--
checkProg src
 = do
	let base	= deFibBase ++ m
	let fileSrc	= base ++ "/Main.ds"
	let fileBin	= base ++ "/Main.bin"
	let fileOut	= base ++ "/out"
	let fileDiff	= base ++ "/out.diff"
	let fileCheck	= base ++ "/out.check"
	
	-- compile
	systemE $ "rm -f " ++ fileOut

	timeCompileStart	<- getClockTime
	systemE $ "bin/ddc"
		++ " -i library "
		++ optFlags
		++ " -m " ++ fileSrc
		++ " -o " ++ fileBin

	timeCompileEnd		<- getClockTime
	out	$ (padL 8 $ pretty $ timeCompileEnd - timeCompileStart)
	

	-- run binary
	timeExecuteStart	<- getClockTime
	systemE $ fileBin ++ " > " ++ fileOut
	timeExecuteEnd		<- getClockTime

	out	$ ",   " 
		% (padL 8 $ pretty $ timeExecuteEnd - timeExecuteStart)
		% "\n"
		

	-- check output
	checkDiff fileOut fileCheck fileDiff

	return ()



{-
-----------------------
-- checkError
-- 	Test out error detection and reporting.
--	We expect the compilation of each of thes modules to FAIL and 
---	report a sensible error message.
--	We diff against the error messages.
--
checkError
 = do
 	putStr "* Running error tests.\n"
	mapM traumaTE errorModules
	putStr "\n"
	
traumaTE m
 = do
 	putStr	$ "  - Testing " ++ m ++ "\n"
	
	let base	= errorBase ++ m
	let fileSrc	= base ++ "/Main.ds"
	let fileOut	= base ++ "/out"
	let fileDiff	= base ++ "/out.diff"
	let fileCheck	= base ++ "/out.check"

	-- compile
	systemE $ "rm -f " ++ fileOut

	systemE	$ "bin/ddc"
		++ optFlags
		++ " -i library"
		++ " -c " ++ fileSrc
		++ " -stop-errors " ++ fileOut
		
		
	-- check output
	checkDiff fileCheck fileOut fileDiff

	return ()


-----------------------
-- checkStaged
--	Test out various sets of language features.
--	These tests are staged so that simpler feaures are tested before more
--	complex ones. Each module prints some output to stdout. This output is
--	collected and diffed with a known good output. Any differences are
--	reported as errors.
--
checkStaged xx
 = do
	putStr "* Running staged tests.           t(comp)     t(exec)\n"
	case xx of
	 []	-> mapM traumaMR stagedModules
	 _	-> mapM traumaMR xx
	 
	putStr "\n"


traumaMR m
 = do
 	out	$ "  - Testing " % (padR 20 m)

	let base	= stagedBase ++ m
	let fileSrc	= base ++ "/Main.ds"
	let fileBin	= base ++ "/Main.bin"
	let fileOut	= base ++ "/out"
	let fileDiff	= base ++ "/out.diff"
	let fileCheck	= base ++ "/out.check"
	
	-- compile
	systemE $ "rm -f " ++ fileOut

	timeCompileStart	<- getClockTime
	systemE $ "bin/ddc"
		++ " -i library "
		++ optFlags
		++ " -m " ++ fileSrc
		++ " -o " ++ fileBin

	timeCompileEnd		<- getClockTime
	out	$ (padL 8 $ pretty $ timeCompileEnd - timeCompileStart)


	-- run binary
	timeExecuteStart	<- getClockTime
	systemE $ fileBin ++ " > " ++ fileOut

	timeExecuteEnd		<- getClockTime
	out	$ ",   " 
		% (padL 8 $ pretty $ timeExecuteEnd - timeExecuteStart)
		% "\n"


	-- check output
	checkDiff fileCheck fileOut fileDiff

	return ()


-----------------------
-- checkDeFib
--	Run the DeFib benchmarks
--
checkDeFib xx  
 = do
	putStr "* Running DeFib tests.            t(comp)     t(exec)\n"
	case xx of
	 []	-> mapM traumaTD deFibModules
	 _	-> mapM traumaTD xx
 	
	putStr "\n"


traumaTD m
 = do
 	out	$ "  - Testing " % (padR 20 m) 

	let base	= deFibBase ++ m
	let fileSrc	= base ++ "/Main.ds"
	let fileBin	= base ++ "/Main.bin"
	let fileOut	= base ++ "/out"
	let fileDiff	= base ++ "/out.diff"
	let fileCheck	= base ++ "/out.check"
	
	-- compile
	systemE $ "rm -f " ++ fileOut

	timeCompileStart	<- getClockTime
	systemE $ "bin/ddc"
		++ " -i library "
		++ optFlags
		++ " -m " ++ fileSrc
		++ " -o " ++ fileBin

	timeCompileEnd		<- getClockTime
	out	$ (padL 8 $ pretty $ timeCompileEnd - timeCompileStart)
	

	-- run binary
	timeExecuteStart	<- getClockTime
	systemE $ fileBin ++ " > " ++ fileOut
	timeExecuteEnd		<- getClockTime

	out	$ ",   " 
		% (padL 8 $ pretty $ timeExecuteEnd - timeExecuteStart)
		% "\n"
		

	-- check output
	checkDiff fileOut fileCheck fileDiff

	return ()
-}

-----------------------
-- checkDiff
--
checkDiff :: FilePath -> FilePath -> FilePath -> IO ()
checkDiff    fileOut     fileCheck   fileDiff
 = do
 	-- check output
	codeDiff 
		<- system $ "diff " ++ fileOut ++ " " ++ fileCheck ++ " > " ++ fileDiff
	
	case codeDiff of
	 ExitSuccess	-> return ()
	 _ -> do
	 	putStr "    . Output differs!\n"
		putStr "\n"
		failDiff fileDiff fileCheck


failDiff fileDiff fileCheck
 = do
	putStr $ "-----------------------\n"
 	putStr $ "-- Diff between " ++ fileDiff 	++ "\n"
	putStr $ "            and " ++ fileCheck 	++ "\n"
	putStr $ "\n"
	
	str	<- readFile fileDiff
	putStr	str
	putStr "\n"
	
	exitFailure


-----
systemE command
 = do
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
		
out s	
 = do	putStr $ pretty s
 	hFlush stdout
	


-----
{-
padRc	:: Char -> Int -> String -> String
padRc	   c       n      str
	= str ++ take (n - length str) (repeat c)

padR n str	= padRc ' ' n str
-}


-----------------------
baseModules = 
	[ "Base/Types"
	, "Base/Thunk"
	, "Data/Ref"
	, "Data/String"
	, "Data/Bool"
	, "Data/Int"
	, "Data/Tuple"
	, "Data/Char"
	, "Data/Maybe"
	, "Data/ArrayU"
	, "System/Error"
	, "System/Console"
	, "Data/List"
	, "Class/Show"
	, "Control/Exception"
	, "Control/Imperative"
	, "Prelude"]


errorBase	= "test/Error/"
errorModules 	=
	[ "Defix/Assoc"
	, "Renamer/Redefined"
	, "Renamer/Undefined"
	, "Patterns/Airity"
	, "Unify/DefConst"
	, "Unify/DefDef"
	, "Unify/FunLess"
	, "Unify/FunMore"
	, "Unify/IfDef"
	, "Unify/IfFun"
	, "Unify/FieldInit"
	, "Grind/FieldNotPresent"
	, "Grind/AmbiguousProjection"
	, "CheckPure/CannotPurify"
	, "CheckConst/ConstWrite"
	, "CheckConst/PureReadWrite" ]


stagedBase	= "test/Staged/"
stagedModules 	= 
	[ "Sanity"
	, "Lazy" 
	, "Except"
	, "Lift"
	, "Apply"
	, "Field"
	, "Syntax"
	, "FullLaziness" ]

deFibBase	= "test/DeFib/"
deFibModules	=
	[ "Ackermann"
	, "Exp3_8"
	, "Primes"
	, "Primes2"
	, "Primes3" 
	, "Tak"
	, "Queens" ]
