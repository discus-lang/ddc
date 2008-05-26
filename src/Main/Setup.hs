
module Main.Setup
	( Setup (..)
	, setupArgs )
where

import Main.Arg
import Util

-- Compile setup 
data Setup
	= Setup
	{ setupArgsCmd		:: [Arg]
	, setupArgsBuild	:: [Arg]
	, setupRuntime		:: FilePath
	, setupLibrary		:: FilePath
	, setupRecursive	:: Maybe [FilePath] }	-- files that we've already compile on this path
	deriving Show

setupArgs setup
	= nub $ setupArgsCmd setup ++ setupArgsBuild setup
