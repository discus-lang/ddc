
module DDC.War.Job
	( Job(..)
	, createJobs)
where
import DDC.War.FileType
import System.FilePath

data Job
	-- | Use DDC to make a program.
	= JobMake {
		-- | Name of the test this job is a part of.
		  jobTestName 	:: String

		-- | Name of the way we're running this test.
		, jobWayName	:: String
		
		-- | Root source file of the program (the 'Main.ds')
		, jobFile	:: FilePath 
		
		-- | Extra DDC options for building in this way.
		, jobOptionsDDC	:: [String] 
		
		-- | Extra GHC RTS options for building in this way.
		, jobOptionsRTS	:: [String]
		
		-- | Scratch dir to do the build in.
		, jobScratchDir	:: String

		-- | Put the compiled binary here.
		, jobMainBin	:: FilePath

		-- | Put what DDC said on stdout here.
		, jobCompileStdout :: FilePath
		
		-- | Put what DDC said out stderr here.
		, jobCompileStderr :: FilePath }
		

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
createJobs :: String -> FilePath -> [Job]
createJobs wayName fileName
 = let	dir		= takeDirectory  fileName
   in	case classifyFile fileName of
	 FileBoring
	  -> []
	
	 -- | For Main.ds files, build and run them.
	 FileMainDS	
	  -> let buildDir		= dir ++ "/war-" ++ wayName
		 mainBin		= buildDir ++ "/Main.bin"
		 mainCompStdout		= buildDir ++ "/Main.compile.stdout"
		 mainCompStderr		= buildDir ++ "/Main.compile.stderr"
		 mainRunStdout		= buildDir ++ "/Main.run.stdout"
		 mainRunStderr		= buildDir ++ "/Main.run.stderr"
	
	     in	 [ JobMake 	dir wayName fileName [] ["-M30M"]
				buildDir mainBin mainCompStdout mainCompStderr 

		 , JobRun  	dir wayName fileName mainBin 
				mainRunStdout mainRunStderr 
		 ]
	
	 FileRunStdoutCheck
	  -> let buildDir		= dir ++ "/war-" ++ wayName
		 mainRunStdout		= buildDir ++ "/Main.run.stdout"
		 mainRunStdoutDiff	= buildDir ++ "/Main.run.stdout.diff"
	     in	 [ JobDiff 	dir wayName fileName mainRunStdout mainRunStdoutDiff]
	
	 FileRunStderrCheck
	  -> let buildDir		= dir ++ "/war-" ++ wayName
		 mainRunStderr		= buildDir ++ "/Main.run.stderr"
		 mainRunStderrDiff	= buildDir ++ "/Main.run.stderr.diff"
	     in	 [ JobDiff 	dir wayName fileName mainRunStderr mainRunStderrDiff ]
	
	 fileType -> error $ "creatJobs: " ++ show fileName ++ " " ++ show fileType
	
