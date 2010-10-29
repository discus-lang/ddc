
module DDC.War.Job
	( Job(..)
	, createJobs)
where
import DDC.War.FileType
import System.FilePath
import qualified Data.Set	as Set
import Data.Set			(Set)

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
		, jobMainBin		:: Maybe FilePath 
		
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
		
	
-- | Create some jobs based on a test file.
--   There can be several jobs associated with a test, depending on what comparison files
--   are available. If any of the jobs in a test fail unexpectedly then the other jobs
--   in that test are ignored.
createJobs 
	:: String 		-- ^ Name of the way we're compiling, used to create build dirs.
	-> Set FilePath		-- ^ All files available. The jobs created for a particular file
				--   like Main.ds depend on the existance of others like Main.error.check
	-> FilePath		-- ^ File we want to create jobs for.
	-> [Job]

createJobs wayName allFiles filePath
 = let	dir		= takeDirectory  filePath
	buildDir	= dir ++ "/war-" ++ wayName
   in	case classifyFile filePath of
	 FileBoring
	  -> []

	 -- For Main.ds files, build and run them.
	 FileMainDS	
	  -> let mainBin	= buildDir ++ "/Main.bin"
		 mainCompStdout	= buildDir ++ "/Main.compile.stdout"
		 mainCompStderr	= buildDir ++ "/Main.compile.stderr"
		 mainCompDiff   = buildDir ++ "/Main.compile.stderr.diff"
		 mainRunStdout	= buildDir ++ "/Main.run.stdout"
		 mainRunStderr	= buildDir ++ "/Main.run.stderr"
		 mainErrorCheck	= dir	   ++ "/Main.error.check"
		 shouldSucceed	= not $ Set.member mainErrorCheck allFiles
	
		 compile 	= JobCompile 	dir wayName filePath [] ["-M30M"]
						buildDir mainCompStdout mainCompStderr 
						(Just mainBin) shouldSucceed

		 run		= JobRun  	dir wayName filePath mainBin 
						mainRunStdout mainRunStderr	
		 
		 diffError	= JobDiff	dir wayName mainErrorCheck
						mainCompStderr mainCompDiff
		
	     in	 [compile] ++ (if shouldSucceed then [run] else [diffError])


	 -- If there is no Main.ds in the same directory, but there is some
	 -- other .ds file, then complile it. If we also have Test.error.check
	 -- then expect failure and check DDC's stdout against the file.
	 -- otherwise just compile it and expect success.
	 FileTestDS
	  -> let (base, _ext)	= splitExtensions $ takeFileName filePath
		 mainDS		= buildDir ++ "/Main.ds"
		 testCompStdout	= buildDir ++ "/" ++ base ++ ".compile.stdout"
		 testCompStderr	= buildDir ++ "/" ++ base ++ ".compile.stderr"
		 testCompDiff   = buildDir ++ "/" ++ base ++ ".compile.stderr.diff"
		 testErrorCheck	= dir 	   ++ "/" ++ base ++ ".error.check"
		 shouldSucceed	= not $ Set.member testErrorCheck allFiles

		 compile	= JobCompile	dir wayName filePath [] ["-M30M"]
						buildDir testCompStdout testCompStderr
						Nothing shouldSucceed
		 
		 diffError	= JobDiff	dir wayName testErrorCheck 
						testCompStderr testCompDiff

		 -- Don't do anything if there is a Main.ds here.
		 -- This other .ds file is probably a part of a larger program.
	     in	 if Set.member mainDS allFiles
		  then []
		  else [compile] ++ (if shouldSucceed then [] else [diffError])


	 -- Run binary was supposed to emit this to stdout.
	 FileRunStdoutCheck
	  -> let mainRunStdout		= buildDir ++ "/Main.run.stdout"
		 mainRunStdoutDiff	= buildDir ++ "/Main.run.stdout.diff"
	     in	 [ JobDiff 	dir wayName filePath mainRunStdout mainRunStdoutDiff]

	
	 -- Run binary was supposed to emit this to stderr.
	 FileRunStderrCheck
	  -> let mainRunStderr		= buildDir ++ "/Main.run.stderr"
		 mainRunStderrDiff	= buildDir ++ "/Main.run.stderr.diff"
	     in	 [ JobDiff 	dir wayName filePath mainRunStderr mainRunStderrDiff ]
	
	
	 -- Expected compile errors are handled by the corresponding FileMainDS or FileTestDS rule.
	 FileCompileErrorCheck	-> []
	
	 fileType -> error $ "creatJobs: " ++ show filePath ++ " " ++ show fileType
	
