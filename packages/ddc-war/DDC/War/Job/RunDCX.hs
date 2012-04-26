
module DDC.War.Job.RunDCX
	( Spec    (..)
        , Result  (..)
        , resultSuccess
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
        { -- | Root source file of the program (the 'Main.ds')
          specFile               :: FilePath 
                
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


resultSuccess :: Result -> Bool
resultSuccess result
 = case result of
        ResultSuccess{} -> True
        _               -> False


instance Pretty Result where
 ppr result 
  = case result of
        ResultSuccess seconds   -> text "success" <+> parens (ppr seconds)
        ResultFailure           -> text "failed"


-- | Compile a Haskell Source File
build :: Spec -> Build Result
build (Spec     srcDCX
		buildDir testRunStdout testRunStderr)

 = do	needs srcDCX
        needs "bin/ddci-core"

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

