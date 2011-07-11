
module DDC.War.FileType
	( FileType(..)
	, classifyFile)
where
import Data.List
import System.FilePath

-- | Classification of a file that is interesting to us.
--   This is a file that we have to build, or might contain output we need to compare against.
data FileType
	= 
	-- | Some file that we don't care about.
	  FileBoring	

	-- | A Main.hs file we should compile with GHC.
	| FileMainHS

	-- | A Main.sh script we should run.
	| FileMainSH

	-- | A Main.ds file we should make into an executable with DDC.
	| FileMainDS

	-- | A Test.ds file we should compile, but not into an executale.
	| FileTestDS

	-- Check errors when compiling
	-- | Compile is expected to fail, and produce some errors into a file.
	--   We check those errors agianst the ones given in this other file.
	| FileCompileErrorCheck

	-- | Compile is expected to fail, and produce some warnings into a file.
	--   We check those warning against the ones given in this other file.
	| FileCompileWarningCheck
	
	-- Check output when running.
	-- | Check the stdout of a program against this.
	| FileRunStdoutCheck

	-- | Check the stderr of a program against this.
	| FileRunStderrCheck
	deriving (Eq, Show)
	

-- | Classify a file name to decide if it's required for the war.
classifyFile :: FilePath -> FileType
classifyFile path
	-- Main files
	| name	== "Main.hs"			= FileMainHS
	| name	== "Main.sh"			= FileMainSH
	| name	== "Main.ds"			= FileMainDS
	
	-- Test compile files
	| (_base, ext) <- splitExtension path
	, ext == ".ds"				= FileTestDS
	
	-- Check errors when compiling.
	| isSuffixOf ".error.check"  name	= FileCompileErrorCheck
	| isSuffixOf ".warn.check"   name	= FileCompileWarningCheck
	
	-- Check output when running programs.
	| isSuffixOf ".stdout.check" name	= FileRunStdoutCheck
	| isSuffixOf ".stderr.check" name	= FileRunStderrCheck
	
	| otherwise				= FileBoring
	where	name	= takeFileName path


