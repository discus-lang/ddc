
module Test where

import Config
import Command
import Timing
import Format

import System.Time
import Control.Monad.Error

-------------------------------------------------------------------------------
data Test
	= TestBuildMain FilePath
	| TestRunBinary FilePath
	deriving (Eq, Ord, Show)


pprTest :: Test -> String
pprTest test
 = case test of	
	TestBuildMain path	-> " * " ++ padR formatPathWidth path ++ " build "
	TestRunBinary path	-> " * " ++ padR formatPathWidth path ++ " run   "



