
module Main.Setup
	( Setup (..)
	, setupArgs 
	, importDirsOfSetup )
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

importDirsOfSetup setup
	= setupLibrary setup
	: (concat $ [dirs | ImportDirs dirs <- setupArgs setup])
	