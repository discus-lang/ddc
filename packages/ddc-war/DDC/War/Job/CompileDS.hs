
module DDC.War.Job.CompileDS
	( Spec         (..)
        , Result       (..)
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox.Build.Benchmark
import BuildBox.Data.Physical
import BuildBox.IO.Directory
import BuildBox.Pretty
import BuildBox
import System.FilePath
import System.Directory
import Control.Monad
import Data.List


-- | Use DDC to compile a source file.
data Spec
        = Spec
        { -- | Root source file of the program (the 'Main.ds')
          specFile               :: FilePath 
                
          -- | Extra DDC options for building in this way.
        , specOptionsDDC         :: [String] 
                
          -- | Extra GHC RTS options for building in this way.
        , specOptionsRTS         :: [String]
                
          -- | Scratch dir to do the build in.
        , specScratchDir         :: String

          -- | Put what DDC says to stdout here.
        , specCompileStdout      :: FilePath
                
          -- | Put what DDC says to stderr here.
        , specCompileStderr      :: FilePath 

          -- | If Just, then we're making an executable, and put the binary here.
          --   Otherwise simply compile it
        , specMaybeMainBin       :: Maybe FilePath 
                
          -- | True if the compile is expected to succeed, else not.
        , specShouldSucceed      :: Bool }
        deriving Show


data Result
        = ResultSuccess Seconds
        | ResultUnexpectedSuccess
        | ResultUnexpectedFailure
        deriving Show


instance Pretty Result where
 ppr result 
  = case result of
        ResultSuccess seconds    -> text "success" <+> parens (ppr seconds)
        ResultUnexpectedFailure  -> text "failed"
        ResultUnexpectedSuccess  -> text "unexpected"


-- | Compile a Disciple source file.
build :: Spec -> Build Result
build (Spec     srcDS optionsDDC optionsRTS
		buildDir mainCompOut mainCompErr
		mMainBin shouldSucceed)

 = do	needs srcDS
	
	-- The directory holding the Main.ds file.
	let (srcDir, _srcFile)	= splitFileName srcDS
		
	-- Touch the .ds files to the build directory to ensure they're built.
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".ds" f || isSuffixOf ".build" f))
		$  lsFilesIn srcDir

	ssystemq $ "touch " ++ (intercalate " " sources)

	-- ensure the output directory exists
	ensureDir buildDir

	-- Do the compile.
	ddcBin'		<- io $ canonicalizePath "bin/ddc"
	let compile
		| Just mainBin	<- mMainBin
		= do	
			-- If there is an existing binary then remove it.
			ssystemq $ "rm -f " ++ mainBin

			-- Build the program.
	 		timeBuild 
	 		 $ systemTee False 
				(ddcBin'
				++ " -v -make "	  ++ srcDS
				++ " -o "	  ++ mainBin
				++ " -outputdir " ++ buildDir
				++ " " 		  ++ intercalate " " optionsDDC
				++ " +RTS "	  ++ intercalate " " optionsRTS)
				""


		-- Compile the program.
		| otherwise
		= do	timeBuild
	 		 $ systemTee False
				(ddcBin'
				++ " -c "	  ++ srcDS
				++ " -outputdir " ++ buildDir
				++ " " 		  ++ intercalate " " optionsDDC
				++ " +RTS "	  ++ intercalate " " optionsRTS)
				""

	(time, (code, strOut, strErr))
		<- compile
	
        atomicWriteFile mainCompOut strOut
        atomicWriteFile mainCompErr strErr

        case code of
         ExitFailure _
          | shouldSucceed       -> return ResultUnexpectedFailure
        
         ExitSuccess
          | not shouldSucceed   -> return ResultUnexpectedSuccess

         _                      -> return $ ResultSuccess time

