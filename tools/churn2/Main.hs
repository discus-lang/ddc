
module Main
where

import Churn.Gen
import Churn.Type
import Churn.Bits
import Churn.Env
import Churn.Chop.Exp

import Source.Exp
import Source.Pretty
import Shared.Pretty
import Util


import System.Cmd
import System.Exit
import qualified Data.Map	as Map
import Data.Map			(Map)


data Config
	= Config
	{ configTmpDir		:: String
	, configLogDir		:: String
	, configFuel		:: Int
	, configRunLimit	:: Maybe Int}

initConfig
	= Config
	{ configTmpDir		= "tmp"
	, configLogDir		= "tmp" 
	, configFuel		= 100
	, configRunLimit	= Just 1 }


-- | run the churner
main 	= churn initConfig 0
 
-- | do a churn pass
churn 	:: Config 	-- churn config
	-> Int 		-- counter for this pass
	-> IO ()
	
churn config ix
 = do	(prog, code)	<- runTest config ix
 	case code of
	 ExitSuccess	-> churn config (ix + 1)
	 ExitFailure _	
	  -> do	stashFailure config ix prog
--	  	churn config (ix + 1)
	  	
	
-- | If the compile fails then stash the offending program.
stashFailure :: Config -> Int -> Tree a -> IO ()
stashFailure config ix tree
 = do
 	-- report failure
 	putStr 	$ "! failed\n"

	-- write the offending program to a new file
	let fileNameLog	
		=   configLogDir config ++ "/"
		++ "failure." ++ show ix ++ ".ds"
		
	writeFile fileNameLog (pprProg tree)
	return ()
	

-- | Generate a random program and compile it.

runTest :: Config	-- churn config
	-> Int 		-- index of test
	-> IO (Tree Type, ExitCode)

runTest config ix
 = do	putStr $ "* running test " ++ show ix ++ "\n"
 	prog		<- runGen (genProg (configFuel config))
	let fileNameDS	= configTmpDir config ++ "/" ++ "test.ds"

 	writeFile fileNameDS (pprProg prog)

	system	$ "rm -f " ++ (configTmpDir config) ++ "/" ++ "test.dump*"
	code	<- system $ "bin/ddc -i library -c " ++ fileNameDS
	return (prog, code)


genProg :: Fuel -> GenM (Tree Type)
genProg fuel
 = do	exp		<- genExp initEnv fuel tInt
	let expPrint	= XApp none (xVar "print") (XApp none (xVar "show") exp)

 	return	$ [PStmt (SBindFun none (varV "main") [WUnit none] expPrint)]



