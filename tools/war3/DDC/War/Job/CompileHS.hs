
module DDC.War.Job.CompileHS
	(jobCompileHS)
where
import DDC.War.Result
import DDC.War.Job
import BuildBox
import System.FilePath
import System.Directory
import Data.ListUtil
import Control.Monad


-- | Compile a Haskell Source File
jobCompileHS :: Job -> Build [Result]
jobCompileHS (JobCompileHS
		testName _wayName srcHS optionsGHC
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

	qssystem $ "cp " ++ (catInt " " sources) ++ " " ++ buildDir

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
	  <- runTimedCommand
	  $  io $ systemTeeIO False
		("make -f " ++ buildMk)
		""
	atomicWriteFile mainCompOut strOut
	atomicWriteFile mainCompErr strErr

	let ftime	= fromRational $ toRational time
	return [ ResultAspect $ Time TotalWall `secs` ftime ]
	
	
genBuildMk :: FilePath -> String -> String -> Build ()
genBuildMk outfile mainBin mainHs
 = do	let str	= "# Generated Makefile\n\n"
 		++ "include make/build.mk\n\n"
                ++ mainBin ++ " : " ++ mainHs ++ "\n"
                ++ "\t$(GHC) $(GHC_LANGUAGE) $(DDC_PACKAGES) -isrc --make $^ -o $@\n\n"
	io $ writeFile outfile str
