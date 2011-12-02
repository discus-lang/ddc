
module DDC.War.JobCreate
	(createJobs)
where
import DDC.War.Config
import DDC.War.Job
import DDC.War.Way
import DDC.War.FileType
import System.FilePath
import qualified Data.Set	as Set
import Data.Set			(Set)

-- | Create some jobs based on a test file.
--   There can be several jobs associated with a test, depending on what comparison files
--   are available. If any of the jobs in a test fail unexpectedly then the other jobs
--   in that test are ignored.
createJobs
	:: Config
	-> Way 			-- ^ Name of the way we're compiling, used to create build dirs.
	-> Set FilePath		-- ^ All files available. The jobs created for a particular file
				--   like Main.ds depend on the existance of others like Main.error.check
	-> FilePath		-- ^ File we want to create jobs for.
	-> [Job]

createJobs config way allFiles filePath
 = let	fileName	= takeFileName filePath
	sourceDir	= takeDirectory  filePath
	buildDir	= sourceDir </> "war-" ++ wayName way
	testName	= sourceDir
   in	case classifyFile filePath of
	 -- Ignore boring files.
	 FileBoring			-> []

	 -- Run stdout and stderr diffs are handled by the FileMainDS rule.
	 FileRunStdoutCheck		-> []
	 FileRunStderrCheck		-> []

	 -- Expected compile errors are handled by the corresponding FileMainDS or FileTestDS rule.
	 FileCompileErrorCheck		-> []

	 -- TODO: Warning tests don't work yet.
	 FileCompileWarningCheck	-> []


	 -- Execute shell scripts -------------------------
	 FileMainSH
	  -> let mainShellStdout	= buildDir  </> "Main.shell.stdout"
		 mainShellStderr 	= buildDir  </> "Main.shell.stderr"
		 mainShellStderrDiff	= buildDir  </> "Main.compile.stderr.diff"
		 mainErrorCheck		= sourceDir </> "Main.error.check"
		 shouldSucceed		= not $ Set.member mainErrorCheck allFiles

		 shell			= JobShell testName (wayName way)
						filePath sourceDir buildDir
						mainShellStdout mainShellStderr
						shouldSucceed

		 diffError		= JobDiff testName (wayName way) mainErrorCheck
						mainShellStderr mainShellStderrDiff

 	     in	[shell] ++ (if shouldSucceed then [] else [diffError])


	 -- For Main.ds files, build and run them with DDC.
	 FileMainDS
	  -> let mainSH	  	  = sourceDir </> "Main.sh"
		 mainBin	  = buildDir  </> "Main.bin"
		 mainCompStdout	  = buildDir  </> "Main.compile.stdout"
		 mainCompStderr	  = buildDir  </> "Main.compile.stderr"
		 mainCompDiff     = buildDir  </> "Main.compile.stderr.diff"
		 mainRunStdout	  = buildDir  </> "Main.run.stdout"
		 mainRunStderr	  = buildDir  </> "Main.run.stderr"

		 mainErrorCheck	  = sourceDir </> "Main.error.check"
		 shouldSucceed	  = not $ Set.member mainErrorCheck allFiles

		 mainStdoutCheck  = sourceDir </> "Main.stdout.check"
		 mainStdoutDiff   = buildDir  </> "Main.run.stdout.diff"
		 shouldDiffStdout = Set.member mainStdoutCheck allFiles

		 mainStderrCheck  = sourceDir </> "Main.stderr.check"
		 mainStderrDiff   = buildDir  </> "Main.run.stderr.diff"
		 shouldDiffStderr = Set.member mainStderrCheck allFiles

		 -- compile the .ds into a .bin
		 compile 	= JobCompile 	testName (wayName way) filePath
		 				(wayOptsComp way) ["-M40M"]
						buildDir mainCompStdout mainCompStderr
						(Just mainBin) shouldSucceed

		 -- run the binary
		 run		= JobRun  	testName (wayName way) filePath mainBin
						mainRunStdout mainRunStderr

		 -- diff errors produced by the compilation
		 diffError	= JobDiff	testName (wayName way) mainErrorCheck
						mainCompStderr mainCompDiff

		 -- diff the stdout of the run
		 diffStdout	= JobDiff	testName (wayName way) mainStdoutCheck
						mainRunStdout mainStdoutDiff

		 -- diff the stderr of the run
		 diffStderr	= JobDiff	testName (wayName way) mainStderrCheck
						mainRunStderr mainStderrDiff

	     in	if Set.member mainSH allFiles
		 then []
		 else [compile]
			++ (if shouldSucceed    then [run]        else [diffError])
			++ (if shouldDiffStdout then [diffStdout] else [])
			++ (if shouldDiffStderr then [diffStderr] else [])


	 -- If there is no Main.ds or Main.sh in the same directory, but there is some
	 -- other .ds file, then compile it. If we also have Test.error.check
	 -- then expect failure and check DDC's stdout against the file.
	 -- otherwise just compile it and expect success.
	 FileTestDS
	  -> let mainDS		= sourceDir </> "Main.ds"
		 mainSH		= sourceDir </> "Main.sh"
		 testCompStdout	= buildDir  </> replaceExtension fileName ".compile.stdout"
		 testCompStderr	= buildDir  </> replaceExtension fileName ".compile.stderr"
		 testCompDiff   = buildDir  </> replaceExtension fileName ".compile.stderr.diff"
		 testErrorCheck	= sourceDir </> replaceExtension fileName ".error.check"
		 shouldSucceed	= not $ Set.member testErrorCheck allFiles

		 compile	= JobCompile	testName (wayName way) filePath
						(wayOptsComp way) ["-M30M"]
						buildDir testCompStdout testCompStderr
						Nothing shouldSucceed

		 diffError	= JobDiff	testName (wayName way) testErrorCheck
						testCompStderr testCompDiff

		 -- Don't do anything if there is a Main.ds here.
		 -- This other .ds file is probably a part of a larger program.
	     in	 if   Set.member mainDS allFiles
		   || Set.member mainSH allFiles
		  then []
		  else [compile] ++ (if shouldSucceed then [] else [diffError])


	 -- For Main.hs files, compile with GHC and run them
	 FileMainHS
	  -> let mainBin	= buildDir </> "Main.bin"
		 mainCompStdout	= buildDir </> "Main.compile.stdout"
		 mainCompStderr	= buildDir </> "Main.compile.stderr"
		 mainRunStdout	= buildDir </> "Main.run.stdout"
		 mainRunStderr	= buildDir </> "Main.run.stderr"

		 compile 	= JobCompileHS 	testName (wayName way) filePath []
						buildDir mainCompStdout mainCompStderr
						mainBin

		 run		= JobRun  	testName (wayName way) filePath mainBin
						mainRunStdout mainRunStderr

	     in	 [compile, run]
