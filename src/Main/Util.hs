module Main.Util
	(compileExit, outVerb)
where

import Control.Monad
import DDC.Main.Pretty
import System.Exit


compileExit :: IO a
compileExit = exitWith ExitSuccess


outVerb :: (?verbose :: Bool) => PrettyM PMode -> IO ()
outVerb ss	= when ?verbose (putStr $ pprStrPlain ss)
