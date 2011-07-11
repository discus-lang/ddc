
module Main.Setup
	( Setup (..)
	, setupArgs 
	, importDirsOfSetup
	, outputDirOfSetup)
where
import DDC.Main.Arg
import Util


-- | Compilation setup, shared between compilation of multiple modules.
--   Carries information about command line arguments,  and where the
--   runtime system and libraries are. 
data Setup
	= Setup
	{ setupArgsCmd		:: [Arg]
	, setupArgsBuild	:: [Arg]
	, setupRuntime		:: FilePath
	, setupLibrary		:: FilePath
	, setupRecursive	:: Maybe [FilePath] }	-- files that we've already compile on this path
	deriving Show


-- | Get all arguments defined by the compile setup.
setupArgs :: Setup -> [Arg]
setupArgs setup
	= nub $ setupArgsCmd setup ++ setupArgsBuild setup


-- | Get import directories of a compile setup.
importDirsOfSetup :: Setup -> [FilePath]
importDirsOfSetup setup
	= setupLibrary setup
	: (concat $ [dirs | ImportDirs dirs <- setupArgs setup])
	

-- | Get directory for output files from a setup.
--   This is where generated files are placed.
outputDirOfSetup :: Setup -> Maybe FilePath
outputDirOfSetup setup
	= listToMaybe
	$ [dir | OutputDir dir <- setupArgs setup]


