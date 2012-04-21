
module DDC.War.Job.RunExe
	( Spec         (..)
        , Result       (..)
        , build)
where
import BuildBox.Build.Benchmark
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Data.Physical
import BuildBox.Pretty
import BuildBox


-- | Run a binary.
data Spec
        = Spec
        { -- | Name of the test this job is a part of.
          specTestName   :: String

          -- | Name of the way we're running this test.
        , specWayName    :: String

          -- | The main source file this binary was built from.
        , specFileSrc    :: FilePath

          -- | Binary to run.
        , specFileBin    :: FilePath 
        
          -- | Put what binary said on stdout here.
        , specRunStdout  :: FilePath
                
          -- | Put what binary said on stderr here.
        , specRunStderr  :: FilePath }
        deriving Show


data Result
        = ResultFailure
        | ResultSuccess Seconds
        deriving Show


instance Pretty Result where
 ppr result 
  = case result of
        ResultSuccess seconds   -> text "success" <+> parens (ppr seconds)
        ResultFailure           -> text "failed"


-- | Run a binary
build :: Spec -> Build Result
build (Spec     testName _wayName _fileName
		mainBin mainRunOut mainRunErr)
 = do	
        needs mainBin
 
	-- Run the binary.
	(time, (code, strOut, strErr))
	 <- timeBuild
	 $  systemTee False mainBin ""

	-- Write its output to files.
	atomicWriteFile mainRunOut strOut
	atomicWriteFile mainRunErr strErr
	
        case code of
         ExitSuccess    -> return $ ResultSuccess time
         _              -> return $ ResultFailure

	
