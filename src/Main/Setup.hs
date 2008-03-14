
module Main.Setup
	( Setup (..)
	, setupZero
	, setupArgs )
where

import Main.Arg
import Util

-- Compile setup 
data Setup
	= Setup
	{ setupArgsCmd		:: [Arg]
	, setupArgsBuild	:: [Arg]
	, setupRecursive	:: Maybe [FilePath] }

setupZero
	= Setup
	{ setupArgsCmd	= []
	, setupArgsBuild	= []
	, setupRecursive	= Nothing }
	

setupArgs setup
	= nub $ setupArgsCmd setup ++ setupArgsBuild setup
