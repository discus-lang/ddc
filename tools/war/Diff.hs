
-- | For checking files via diff.
module Diff 
	(checkDiff)

where 

import Interface
import Util

import System.Cmd
import System.Exit
import System.IO

-----


-- | Diff these files and if they are not the same then ask the user if this is ok, and what to do.
--	options are, 
--		update the check file
--		end the program
--		skip the problem
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



