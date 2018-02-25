
module DDC.War.Create.CreateDSX
        (create)
where
import DDC.War.Create.Way
import DDC.War.Driver
import System.FilePath
import DDC.War.Job                              ()
import Data.Set                                 (Set)
import qualified DDC.War.Job.RunDSX             as RunDSX
import qualified DDC.War.Job.Diff               as Diff
import qualified Data.Set                       as Set


-- | Run .dcx files with the interpreter.
create :: Way -> Set FilePath -> FilePath -> Maybe Chain
create way allFiles filePath
 | takeFileName filePath == "Test.dsx"
 = let  
        fileName         = takeFileName filePath
        sourceDir        = takeDirectory  filePath
        buildDir         = sourceDir </> "war-" ++ wayName way
        testName         = filePath

        testDDCiStdout   = buildDir  </> replaceExtension fileName ".ddci-tetra.stdout"
        testDDCiStderr   = buildDir  </> replaceExtension fileName ".ddci-tetra.stderr"

        testStdoutCheck  = sourceDir </> "Test.stdout.check"
        testStdoutDiff   = buildDir  </> "Test.stdout.check.diff"
        shouldDiffStdout = Set.member testStdoutCheck allFiles

        jobRun           = jobOfSpec (JobId testName (wayName way))
                         $ RunDSX.Spec
                                filePath
                                buildDir testDDCiStdout testDDCiStderr

        jobDiff          = jobOfSpec (JobId testName (wayName way))
                         $ Diff.Spec
                                testStdoutCheck
                                testDDCiStdout testStdoutDiff

   in   Just $ Chain 
                $  [jobRun] 
                ++ (if shouldDiffStdout then [jobDiff] else [])

 | otherwise    = Nothing
