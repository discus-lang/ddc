
module Main.Setup
	( Setup (..)
	, setupZero
	, setupArgs )
where

import Main.Arg

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
	= setupArgsCmd setup ++ setupArgsBuild setup
