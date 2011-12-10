
module DDC.War.Job
	( Job(..))
where

data Job
	-- | Use DDC to compile/make file.
	= JobCompile {
		-- | Name of the test this job is a part of.
		  jobTestName		:: String

		-- | Name of the way we're running this test.
		, jobWayName		:: String
		
		-- | Root source file of the program (the 'Main.ds')
		, jobFile		:: FilePath 
		
		-- | Extra DDC options for building in this way.
		, jobOptionsDDC		:: [String] 
		
		-- | Extra GHC RTS options for building in this way.
		, jobOptionsRTS		:: [String]
		
		-- | Scratch dir to do the build in.
		, jobScratchDir		:: String

		-- | Put what DDC says to stdout here.
		, jobCompileStdout	:: FilePath
		
		-- | Put what DDC says to stderr here.
		, jobCompileStderr	:: FilePath 

		-- | If Just, then we're making an executable, and put the binary here.
		--   Otherwise simply compile it
		, jobMaybeMainBin	:: Maybe FilePath 
		
		-- | True if the compile is expected to succeed, else not.
		, jobShouldSucceed	:: Bool }


	-- | Use GHC to compile/make file.
	| JobCompileHS {
		-- | Name of the test this job is a part of.
		  jobTestName		:: String

		-- | Name of the way we're running this test.
		, jobWayName		:: String
		
		-- | Root source file of the program (the 'Main.ds')
		, jobFile		:: FilePath 
		
		-- | Extra DDC options for building in this way.
		, jobOptionsGHC		:: [String] 
				
		-- | Scratch dir to do the build in.
		, jobScratchDir		:: String

		-- | Put what DDC says to stdout here.
		, jobCompileStdout	:: FilePath
		
		-- | Put what DDC says to stderr here.
		, jobCompileStderr	:: FilePath 

		-- | If Just, then we're making an executable, and put the binary here.
		--   Otherwise simply compile it
		, jobMainBin		:: FilePath }


	-- | Feed a file into DDCi-core
	| JobRunDCX {
		-- | Name of the test this job is a part of.
		  jobTestName		:: String

		-- | Name of the way we're running this test.
		, jobWayName		:: String
		
		-- | Root source file of the program (the 'Main.ds')
		, jobFile		:: FilePath 
		
		-- | Scratch dir to do the build in.
		, jobScratchDir		:: String

		-- | Put what DDC says to stdout here.
		, jobCompileStdout	:: FilePath
		
		-- | Put what DDC says to stderr here.
		, jobCompileStderr	:: FilePath }


	-- | Run a shell script.
	| JobShell {
		-- | Name of the test this job is a part of.
		  jobTestName		:: String

		-- | Name of the way we're running this test.
		, jobWayName		:: String

		-- | Shell script to run
		, jobShellSource	:: FilePath

		-- | Source dir that the script is in.
		, jobSourceDir		:: FilePath

		-- | Scratch dir that the script can write files to.
		, jobScratchDir		:: FilePath

		-- | Put what DDC says to stdout here.
		, jobShellStdout	:: FilePath
		
		-- | Put what DDC says to stderr here.
		, jobShellStderr	:: FilePath 

		-- | True if the compile is expected to succeed, else not.
		, jobShouldSucceed	:: Bool }


	-- | Run a binary.
	| JobRun {
		-- | Name of the test this job is a part of.
		  jobTestName	:: String

		-- | Name of the way we're running this test.
		, jobWayName	:: String

		-- | The main source file this binary was built from.
		, jobFileSrc	:: FilePath

		-- | Binary to run.
		, jobFileBin	:: FilePath 
	
		-- | Put what binary said on stdout here.
		, jobRunStdout	:: FilePath
		
		-- | Put what binary said on stderr here.
		, jobRunStderr	:: FilePath }


	-- | Diff two files.
	| JobDiff { 
		-- | Name of the test this job is a part of.
		  jobTestName	:: String

		-- | Name of the way we're running this test.
		, jobWayName	:: String

		-- | The baseline file.
		, jobFile	:: FilePath 
		
		-- | File produced that we want to compare with the baseline.
		, jobFileOut	:: FilePath 
		
		-- | Put the result of the diff here.
		, jobFileDiff	:: FilePath }
		
	deriving (Show)
		
	
