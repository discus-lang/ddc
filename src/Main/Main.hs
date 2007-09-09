
module Main
(	
	module Util.List,
)


where

-----
import qualified System.Posix.Temp	as System
import qualified System.Environment	as System
import qualified System.IO		as System
import qualified System.Directory	as System
import qualified System

-----
import Util.List
import Util.Monad hiding (lift)
import Util.Maybe
import Util.Pretty
import Util.Misc

-----
import qualified Shared.Var 		as Var
import Shared.Pretty

import Main.Version
import qualified Main.Arg 		as Arg
import Main.Arg (Arg)
import Main.Compile


import Module.Pretty
import qualified Module.Interface	as MI

-----
import qualified Stages.IO		as IO
import qualified Stages.Source		as Source

------
-- Main program
--
--
main :: IO ()
main 
 = do
	-- check args
 	argStrings	<- System.getArgs
	let args	= Arg.parse $ catInt " " argStrings

	let verbose	= or $ map (== Arg.Verbose) args

	-- print banner if requested
	when verbose
	 (do
	 	putStr	$ "* Disciplined Disciple Compiler " ++ version ++ " starting up...\n"
		putStr	$ "  options:\n"

		putStr  $ concat 
			$ map (\s -> "    " ++ s ++ "\n") 
			$ map show args)

	

	-- no args, print help
	when (args == []
	   || elem Arg.Help args)
	 (do
		putStr Arg.helpString
		System.exitWith System.ExitSuccess)


	-- bad args, bail out
	when (or $ map (=@= Arg.Error{}) args)
	 (do
		let eArg	= head $ filter (=@= Arg.Error{}) args
		let eString 	= case eArg of { Arg.Error x -> x; }

	 	putStr ("* Error - bad argument: '" ++ eString ++ "'\n")
		
		System.exitFailure
	 )


	-- gather up list of files to compile
	let compileFiles	= concat
				$ map (\a -> case a of {
						Arg.Compile fs  -> fs;
						_		-> [] })
				$ args
				
	
	-- no input files, bail out
	when (compileFiles == [])
	 (do	putStr ("* Error - no input files\n")
		System.exitFailure)


	-- compile input files	
	mapM (compileFile args) compileFiles

	-- emit a blank line to make things look nicer
	when verbose
		(putStr "\n")
	
	-- sweet success
	System.exitWith System.ExitSuccess

