
module DDC.War.Job.Shell
	( Spec     (..)
        , Result   (..)
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox


-- | Run a shell script.
data Spec
        = Spec
        { -- | Name of the test this job is a part of.
          specTestName           :: String

          -- | Name of the way we're running this test.
        , specWayName            :: String

          -- | Shell script to run
        , specShellSource        :: FilePath

          -- | Source dir that the script is in.
        , specSourceDir          :: FilePath

          -- | Scratch dir that the script can write files to.
        , specScratchDir         :: FilePath

          -- | Put what DDC says to stdout here.
        , specShellStdout        :: FilePath
                
          -- | Put what DDC says to stderr here.
        , specShellStderr        :: FilePath 

          -- | True if the compile is expected to succeed, else not.
        , specShouldSucceed      :: Bool }


data Result
        = ResultSuccess
        | ResultUnexpectedSuccess
        | ResultUnexpectedFailure
        deriving Show


-- | Run a binary
build :: Spec -> Build Result
build   (Spec   testName _wayName
		mainSH sourceDir scratchDir
		mainRunOut mainRunErr
		shouldSucceed)
 = do	
        needs mainSH
	ensureDir scratchDir
	
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- timeBuild
	 $  systemTee False 
		("sh " ++ mainSH ++ " " ++ sourceDir ++ " " ++ scratchDir) 
		""
		
	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
		
        case code of
         ExitSuccess
          | shouldSucceed       -> return ResultSuccess
          | otherwise           -> return ResultUnexpectedSuccess

         ExitFailure _
          | shouldSucceed       -> return ResultUnexpectedFailure
          | otherwise           -> return ResultSuccess

