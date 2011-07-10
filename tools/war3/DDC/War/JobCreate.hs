
module DDC.War.JobCreate
	(createJobs)
where
import DDC.War.Job
import DDC.War.FileType
import System.FilePath
import qualified Data.Set	as Set
import Data.Set			(Set)

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
 = let	fileName	= takeFileName filePath
	sourceDir	= takeDirectory  filePath
	buildDir	= sourceDir </> "war-" ++ wayName
	testName	= sourceDir
   in	case classifyFile filePath of
	 FileBoring
	  -> []

	 -- For Main.ds files, build and run them with DDC.
	 FileMainDS	
	  -> let mainBin	= buildDir  </> "Main.bin"
		 mainCompStdout	= buildDir  </> "Main.compile.stdout"
		 mainCompStderr	= buildDir  </> "Main.compile.stderr"
		 mainCompDiff   = buildDir  </> "Main.compile.stderr.diff"
		 mainRunStdout	= buildDir  </> "Main.run.stdout"
		 mainRunStderr	= buildDir  </> "Main.run.stderr"
		 mainErrorCheck	= sourceDir </> "Main.error.check"
		 shouldSucceed	= not $ Set.member mainErrorCheck allFiles
	
		 compile 	= JobCompile 	testName wayName filePath [] ["-M30M"]
						buildDir mainCompStdout mainCompStderr 
						(Just mainBin) shouldSucceed

		 run		= JobRun  	testName wayName filePath mainBin 
						mainRunStdout mainRunStderr	
		 
		 diffError	= JobDiff	testName wayName mainErrorCheck
						mainCompStderr mainCompDiff
		
	     in	 [compile] ++ (if shouldSucceed then [run] else [diffError])


	 -- If there is no Main.ds in the same directory, but there is some
	 -- other .ds file, then complile it. If we also have Test.error.check
	 -- then expect failure and check DDC's stdout against the file.
	 -- otherwise just compile it and expect success.
	 FileTestDS
	  -> let mainDS		= sourceDir </> "Main.ds"
		 testCompStdout	= buildDir  </> replaceExtension fileName ".compile.stdout"
		 testCompStderr	= buildDir  </> replaceExtension fileName ".compile.stderr"
		 testCompDiff   = buildDir  </> replaceExtension fileName ".compile.stderr.diff"
		 testErrorCheck	= sourceDir </> replaceExtension fileName ".error.check"
		 shouldSucceed	= not $ Set.member testErrorCheck allFiles

		 compile	= JobCompile	testName wayName filePath [] ["-M30M"]
						buildDir testCompStdout testCompStderr
						Nothing shouldSucceed
		 
		 diffError	= JobDiff	testName wayName testErrorCheck 
						testCompStderr testCompDiff

		 -- Don't do anything if there is a Main.ds here.
		 -- This other .ds file is probably a part of a larger program.
	     in	 if Set.member mainDS allFiles
		  then []
		  else [compile] ++ (if shouldSucceed then [] else [diffError])

	 -- For Main.hs files, compile with GHC and run them
	 FileMainHS
	  -> let mainBin	= buildDir </> "Main.bin"
		 mainCompStdout	= buildDir </> "Main.compile.stdout"
		 mainCompStderr	= buildDir </> "Main.compile.stderr"
		 mainRunStdout	= buildDir </> "Main.run.stdout"
		 mainRunStderr	= buildDir </> "Main.run.stderr"

		 compile 	= JobCompileHS 	testName wayName filePath []
						buildDir mainCompStdout mainCompStderr 
						mainBin

		 run		= JobRun  	testName wayName filePath mainBin 
						mainRunStdout mainRunStderr	
		
	     in	 [compile, run]


	 -- Run binary was supposed to emit this to stdout.
	 FileRunStdoutCheck
	  -> let mainRunStdout		= buildDir </> "Main.run.stdout"
		 mainRunStdoutDiff	= buildDir </> "Main.run.stdout.diff"
	     in	 [ JobDiff 	testName wayName filePath mainRunStdout mainRunStdoutDiff]

	
	 -- Run binary was supposed to emit this to stderr.
	 FileRunStderrCheck
	  -> let mainRunStderr		= buildDir </> "Main.run.stderr"
		 mainRunStderrDiff	= buildDir </> "Main.run.stderr.diff"
	     in	 [ JobDiff 	testName wayName filePath mainRunStderr mainRunStderrDiff ]
	

	 FileMainSH
	  -> let mainShellStdout	= buildDir  </> "Main.shell.stdout"
		 mainShellStderr 	= buildDir  </> "Main.shell.stderr"
	     
		 shell			= JobShell testName wayName 
						filePath sourceDir buildDir
						mainShellStdout mainShellStderr
						True

 	     in	[shell]

	
	 -- Expected compile errors are handled by the corresponding FileMainDS or FileTestDS rule.
	 FileCompileErrorCheck		-> []
	
	 -- These tests don't work yet.
	 FileCompileWarningCheck	-> []
	
