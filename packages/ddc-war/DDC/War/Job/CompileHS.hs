
module DDC.War.Job.CompileHS
	( Spec         (..)
        , Result       (..)
        , resultSuccess
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


-- | Use GHC to compile/make file.
data Spec
        = Spec 
        { -- | Root source file of the program (the 'Main.ds')
          specFile               :: FilePath 
                
          -- | Extra DDC options for building in this way.
        , specOptionsGHC         :: [String] 
                                
          -- | Scratch dir to do the build in.
        , specScratchDir         :: String

          -- | Put what DDC says to stdout here.
        , specCompileStdout      :: FilePath
                
          -- | Put what DDC says to stderr here.
        , specCompileStderr      :: FilePath 

          -- | If Just, then we're making an executable, and put the binary here.
          --   Otherwise simply compile it
        , specMainBin            :: FilePath }
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
build  (Spec    srcHS _optionsGHC
		buildDir mainCompOut mainCompErr
		mainBin)

 = do	needs srcHS
	
	-- The directory holding the Main.hs file.
	let (srcDir, srcFile)	= splitFileName srcHS
		
	-- Copy the .hs files to the build directory.
	-- This freshens them and ensures we won't conflict with other make jobs
	-- running on the same source files, but in different ways.
	ensureDir buildDir
	sources	<- io
		$  liftM (filter (\f -> isSuffixOf ".hs" f))
		$  lsFilesIn srcDir

	_ <- ssystemq $ "cp " ++ (intercalate " " sources) ++ " " ++ buildDir

	-- The copied version of the root source file.
	let srcCopyHS	= buildDir ++ "/" ++ srcFile
	let buildMk	= buildDir ++ "/build.mk"
	
	-- We're doing this via a makefile so we get the ghc flags and options
	-- from the DDC build system. The paths in the make file need to be in
	-- posix form with '/' as the separators.
	currentDir	<- io $ getCurrentDirectory
	let hacks f	= map (\z -> if z == '\\' then '/' else z) f
	let mainBin'	= hacks $ makeRelative currentDir mainBin
	let srcCopyHS'	= hacks $ makeRelative currentDir srcCopyHS
	
	genBuildMk buildMk mainBin' srcCopyHS'
	
	(time, (code, strOut, strErr))
	  <- timeBuild
	  $  systemTee False
		("make -f " ++ buildMk)
		""
	atomicWriteFile mainCompOut strOut
	atomicWriteFile mainCompErr strErr

        case code of
         ExitSuccess    -> return $ ResultSuccess time
         ExitFailure _  -> return ResultFailure
		

genBuildMk :: FilePath -> String -> String -> Build ()
genBuildMk outfile mainBin mainHs
 = do	let str	= "# Generated Makefile\n\n"
 		++ "include make/build.mk\n\n"
                ++ mainBin ++ " : " ++ mainHs ++ "\n"
                ++ "\t$(GHC) $(GHC_LANGUAGE) $(DDC_PACKAGES) -ipackages/ddc-alpha -ipackages/ddc-core-llvm --make $^ -o $@\n\n"
	io $ writeFile outfile str

