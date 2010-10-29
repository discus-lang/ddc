
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

createJobs wayName allFiles fileName
 = let	dir		= takeDirectory  fileName
	buildDir	= dir ++ "/war-" ++ wayName
   in	case classifyFile fileName of
	 FileBoring
	  -> []

	 -- | Test.ds files should be compiled.
	 --   If we also have Test.error.check then expect failure and check DDC's stdout against the file.
	 --   otherwise just compile it and expect success.
	 FileTestDS
	  -> let testCompStdout		= buildDir ++ "/Test.compile.stdout"
		 testCompStderr		= buildDir ++ "/Test.compile.stderr"
		 testErrorCheck		= dir 	   ++ "/Test.error.check"
		 shouldSucceed		= not $ Set.member testErrorCheck allFiles

	     in	 [ JobCompile	dir wayName fileName [] ["-M30M"]
				buildDir testCompStdout testCompStderr
				Nothing shouldSucceed ]

	 -- | For Main.ds files, build and run them.
	 FileMainDS	
	  -> let mainBin		= buildDir ++ "/Main.bin"
		 mainCompStdout		= buildDir ++ "/Main.compile.stdout"
		 mainCompStderr		= buildDir ++ "/Main.compile.stderr"
		 mainRunStdout		= buildDir ++ "/Main.run.stdout"
		 mainRunStderr		= buildDir ++ "/Main.run.stderr"
		 mainErrorCheck		= dir	   ++ "/Main.error.check"
		 shouldSucceed		= not $ Set.member mainErrorCheck allFiles
	
	     in	[ JobCompile 	dir wayName fileName [] ["-M30M"]
				buildDir mainCompStdout mainCompStderr 
				(Just mainBin) shouldSucceed

		, JobRun  	dir wayName fileName mainBin 
				mainRunStdout mainRunStderr ]
		
	 FileRunStdoutCheck
	  -> let mainRunStdout		= buildDir ++ "/Main.run.stdout"
		 mainRunStdoutDiff	= buildDir ++ "/Main.run.stdout.diff"
	     in	 [ JobDiff 	dir wayName fileName mainRunStdout mainRunStdoutDiff]
	
	 FileRunStderrCheck
	  -> let mainRunStderr		= buildDir ++ "/Main.run.stderr"
		 mainRunStderrDiff	= buildDir ++ "/Main.run.stderr.diff"
	     in	 [ JobDiff 	dir wayName fileName mainRunStderr mainRunStderrDiff ]
	
	 -- | Expected compile errors are handled by the corresponding FileMainDS or FileTestDS rule.
	 FileCompileErrorCheck
	  -> []
	
	 fileType -> error $ "creatJobs: " ++ show fileName ++ " " ++ show fileType
	
