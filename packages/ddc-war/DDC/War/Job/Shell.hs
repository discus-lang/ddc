
module DDC.War.Job.Shell
	( Spec     (..)
        , Result   (..)
        , resultSuccess
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.Data.Physical
import BuildBox.Pretty
import BuildBox


-- | Run a shell script.
data Spec
        = Spec
        { -- | Shell script to run
          specShellSource        :: FilePath

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
        deriving Show


data Result
        = ResultSuccess Seconds
        | ResultUnexpectedSuccess
        | ResultUnexpectedFailure
        deriving Show


resultSuccess :: Result -> Bool
resultSuccess result
 = case result of
        ResultSuccess{} -> True
        _               -> False


instance Pretty Result where
 ppr result 
  = case result of
        ResultSuccess seconds   -> text "success" <+> parens (ppr seconds)
        ResultUnexpectedFailure -> text "failed"
        ResultUnexpectedSuccess -> text "unexpected"


-- | Run a binary
build :: Spec -> Build Result
build   (Spec   mainSH sourceDir scratchDir
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
          | shouldSucceed       -> return $ ResultSuccess time
          | otherwise           -> return ResultUnexpectedSuccess

         ExitFailure _
          | shouldSucceed       -> return ResultUnexpectedFailure
          | otherwise           -> return $ ResultSuccess time

