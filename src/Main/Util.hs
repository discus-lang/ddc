module Main.Util
	(compileExit, outVerb)
where

import Control.Monad
import DDC.Main.Pretty
import System.Exit


compileExit :: IO a
compileExit = exitWith ExitSuccess


outVerb :: (?verbose :: Bool) => Str -> IO ()
outVerb ss	= when ?verbose (putStr $ pprStrPlain ss)
