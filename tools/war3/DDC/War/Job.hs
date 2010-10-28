
module DDC.War.Job
	( Job(..)
	, createJobs)
where
import DDC.War.FileType
import System.FilePath

data Job
	-- | Use DDC to make a program.
	= JobMake 
		{ jobName 	:: String

		-- | Root source file of the program (the 'Main.ds')
		, jobFile	:: FilePath 
		
		-- | Extra DDC options for building in this way.
		, jobOptionsDDC	:: [String] 
		
		-- | Extra GHC RTS options for building in this way.
		, jobOptionsRTS	:: [String]
		
		-- | Scratch dir to do the build in.
		, jobScratchDir	:: String

		-- | Where to write the compiled binary.
		, jobMainBin	:: FilePath

		-- | Where to write what DDC said to stdout
		, jobCompileStdout :: FilePath
		
		-- | Where to write what DDC said to stderr
		, jobCompileStderr :: FilePath }
		

	-- | Run a binary.
	| JobRun
		{ jobName	:: String
		, jobFile	:: FilePath }

	-- | Diff two files.
	| JobDiff
		{ jobName	:: String
		, jobFile	:: FilePath 
		, jobFileDirr	:: FilePath }
		
	deriving (Show)
	
	
-- | Create some jobs based on a test file.
createJobs :: FilePath -> [Job]
createJobs fileName
 = let	dir		= takeDirectory  fileName
   in	case classifyFile fileName of
	 FileBoring
	  -> []
	
	 FileMainDS	
	  -> let buildDir	= dir ++ "/war-normal"
		 mainBin	= buildDir ++ "/Main.bin"
		 mainCompStdout	= buildDir ++ "/Main.compile.stdout"
		 mainCompStderr = buildDir ++ "/Main.compile.stderr"
	
	     in	 [ JobMake 	dir fileName [] ["-M30M"]
				buildDir mainBin mainCompStdout mainCompStderr 

		 , JobRun  	dir mainBin ]
	
	 FileRunStdoutCheck
	  ->	[ JobDiff dir fileName (dir ++ "/Main.stdout") ]
	
	 FileRunStderrCheck
	  -> 	[ JobDiff dir fileName (dir ++ "/Main.stderr") ]
	
			

	 fileType -> error $ "creatJobs: " ++ show fileName ++ " " ++ show fileType
	
