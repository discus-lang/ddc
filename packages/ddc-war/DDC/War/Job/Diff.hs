
module DDC.War.Job.Diff
	( Spec         (..)
        , Result       (..) 
        , build)
where
import BuildBox.Command.File
import BuildBox.Command.System
import BuildBox


-- | Diff two files.
data Spec
        = Spec
        { -- | Name of the test this job is a part of.
          specTestName   :: String

          -- | Name of the way we're running this test.
        , specWayName    :: String

          -- | The baseline file.
        , specFile       :: FilePath 
                
          -- | File produced that we want to compare with the baseline.
        , specFileOut    :: FilePath 
                
          -- | Put the result of the diff here.
        , specFileDiff   :: FilePath }


data Result
        = ResultSame

        | ResultDiff 
        { resultFileRef  :: FilePath
        , resultFileOut  :: FilePath
        , resultFileDiff :: FilePath }


-- | Compare two files for differences.
build :: Spec -> Build Result
build (Spec     testName _wayName 
		fileRef fileOut fileDiff)
 = do	needs fileRef
	needs fileOut
	
	let diffExe	= "diff"
	
	-- Run the binary.
	(code, strOut, strErr)
	 <- systemTee False 
	 	(diffExe ++ " " ++ fileRef ++ " " ++ fileOut)
		""
	
	-- Write its output to file.
	atomicWriteFile fileDiff strOut

	if strOut == ""
	 then return $ ResultSame
	 else return $ ResultDiff fileRef fileOut fileDiff

