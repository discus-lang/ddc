
module DDC.War.Job.RunDCX
	( Spec    (..)
        , Result  (..)
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.Data.Physical
import BuildBox.Pretty
import BuildBox
import System.Directory


-- | Feed a file into DDCi-core        
data Spec
        = Spec
        { -- | Name of the test this job is a part of.
          specTestName           :: String

          -- | Name of the way we're running this test.
        , specWayName            :: String
                
          -- | Root source file of the program (the 'Main.ds')
        , specFile               :: FilePath 
                
          -- | Scratch dir to do the build in.
        , specScratchDir         :: String

          -- | Put what DDC says to stdout here.
        , specCompileStdout      :: FilePath
                
          -- | Put what DDC says to stderr here.
        , specCompileStderr      :: FilePath }
        deriving Show


data Result
        = ResultSuccess Seconds
        | ResultFailure
        deriving Show


instance Pretty Result where
 ppr result 
  = case result of
        ResultSuccess _time     -> text "success"
        ResultFailure           -> text "failure"


-- | Compile a Haskell Source File
build :: Spec -> Build Result
build (Spec     testName _wayName srcDCX
		buildDir testRunStdout testRunStderr)

 = do	needs srcDCX

	-- ensure the output directory exists
	ensureDir buildDir

	ddciBin' <- io $ canonicalizePath "bin/ddci-core"

	(time, (code, strOut, strErr))
	  <- timeBuild
	  $  systemTee False
		(ddciBin' ++ " --batch " ++ srcDCX)
		""
	atomicWriteFile testRunStdout strOut
	atomicWriteFile testRunStderr strErr

        case code of
         ExitSuccess    -> return $ ResultSuccess time
         _              -> return $ ResultFailure

