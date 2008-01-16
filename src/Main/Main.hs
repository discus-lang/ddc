
module Main
	( main
	, ddc
	, module Util.List )
where

import Main.Version
import qualified Main.Arg 		as Arg
import Main.Arg (Arg)
import Main.Compile

import qualified Main.IO		as IO
import qualified Main.Source		as Source

import Util.List
import Util.Monad hiding (lift)
import Util.Misc
import qualified System.IO		as System
import qualified System

------
-- Main program
--
--
main :: IO ()
main	
 = do	argStrings	<- System.getArgs
 	ddc argStrings

ddc :: [String] -> IO ()
ddc argStrings
 = do
	-- check args
	let args	= (Arg.parse $ catInt " " argStrings)
			++ [Arg.LintCore]

	let verbose	= or $ map (== Arg.Verbose) args

	-- print banner if requested
	when verbose
	 (do 	putStr	$ "* Disciplined Disciple Compiler " ++ version ++ " starting up...\n")

	
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

