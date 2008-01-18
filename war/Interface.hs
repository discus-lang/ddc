
-- | Low Level functions for interfacing with the user
module Interface
	(out, systemE)
	
where

import Util

import System.IO
import System.Cmd
import System.Exit


-- | Write something to the console
out :: Pretty a => a -> IO ()
out s	
 = do	putStr $ pprStr s
 	hFlush stdout


-- | Run a system command and bail out if it returns an error
systemE :: (?trace :: PrettyP -> IO ()) 
	=> String -> IO ()
systemE command
 = do
	?trace	$ "* systemE " % command	% "\n"
 	code	<- system command
	case code of
	 ExitFailure _	-> exitFailure
	 _		-> return ()
